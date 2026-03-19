# Mooncaml – How to Add a New Feature (End-to-End Workflow)

The mental model is:

```
Client (TCP) 
  ↓ sexp packets
packets.ml               ← protocol vocabulary (shared)
  ↓ t variant
client.ml                ← application logic / orchestration / state machine
  ↕ domain types / Entities.player
repository.ml            ← DB boundary + error normalization + integrity checks
  ↓ raw tuples / scalars
queries.ml               ← pure SQL + Caqti type sigs
  ↓
PostgreSQL
```

When adding any new feature, follow this repeatable sequence. Work **top-down for design**, **bottom-up for implementation**.

### Step 0 – Classify the Feature

- **Pure gameplay** (no persistence): emotes, temporary effects → only `packets.ml` + `client.ml`
- **Persistent** (DB write/read): inventory, stats, bio, position → full pipeline
- **Hybrid** (most common): chat, movement with save, cooldowns → full pipeline + careful Entities vs Models distinction

### Step 1 – Define the Protocol (`lib/shared/packets.ml`)

Add variants using the **Command / Response / Event** convention:

- **Command** = client intent (minimal data)
- **Response** = feedback only to the sender `(bool * string)` or structured
- **Event** = what actually happened (broadcast, usually includes resolved IDs/names)

Example (private message):

```ocaml
type t =
  (* … existing … *)
  | WhisperCommand     of { target_name : string;  message : string }
  | WhisperEvent       of { sender_name : string;  message : string }
  | WhisperCommandResponse of (bool * string)
```

Thanks to `[@@deriving sexp]`, serialization is free and the wire format is readable.

**Rule of thumb**  
Keep Commands small/intent-focused.  
Events contain what everyone needs to know (usually resolved names, not secret IDs).

### Step 2 – Update Domain / Shared Types (if needed)

- **`lib/shared/entities.ml`** – things every client must know at runtime (`player` record)  
  → ephemeral `id` = network client ID  
  → **never** leak DB `user_id` / `entity_id` here

- **`lib/server/models.ml`** (if you have it) – persistence-oriented types (closer to DB shape)

Most features only touch `Entities.player`.
  
  > ### What is `Entities.player`?
  > 
  > `Entities.player` is a **network-facing type**, not a database model and not the full server game object.
  > 
  > It represents:
  > - what the client is allowed to see
  > - what gets sent over packets
  > 
  > It must NOT contain:
  > - database IDs (`user_id`, `entity_id`)
  > - hidden/internal state
  > - sensitive data
  > 
  > Think of it as a **DTO (Data Transfer Object)**.

### Step 3 – Write SQL (`lib/server/db/queries.ml`)

Keep this file **dumb and pure**:

- One focused SQL string per `let`
- Caqti type DSL for input/output
- No logic, no branching, no error handling

```ocaml
let get_player_position_by_user_id =
  (Caqti_type.int ->? Caqti_type.(t2 int int))
  @@ {sql|
    SELECT e.x, e.y
    FROM players p
    JOIN entities e ON p.entity_id = e.id
    WHERE p.user_id = $1;
  |sql}
;;
```

**Strong preference**: many small single-purpose queries > few huge joins.

### Step 4 – Create Repository Function (`lib/server/db/repository.ml`)

The Repository is the **Firewall**. It normalizes errors and enforces **Existence Expectations**.

When implementing a lookup, ask yourself: **"Is it okay if this is missing?"**
1.  **Lookups (Option)**: If "not found" is a valid business state (e.g., checking if a username is taken), return `Ok (Some x) | Ok None`.
2.  **Integrity Checks (Flat)**: If "not found" implies a bug or invalid state (e.g., getting the position of a logged-in player), map `None` to `Error`.

```ocaml
let get_player_position pool user_id =
  let* res = pool |> Caqti_lwt_unix.Pool.use (fun (module Db) ->
      Db.find_opt Queries.get_player_position_by_user_id user_id)
  in
  Lwt.return @@ match unwrap res with
  | Ok (Some (x, y)) -> Ok (x, y)
  | Ok None          -> Error "Integrity Error: Player record missing"
  | Error msg        -> Error msg
```

### Step 5 – Handle the Packet (`lib/server/client.ml`)

This is the **orchestration layer** — think of it as readable story code.

Typical handler shape (in `StateInGame` / `StateInLobby`):

```ocaml
let handle_chat_command
        client
        (player : Entities.player)
        user_id
        player_entity_id
        (packet : Packet.t)
    =
    match packet with
    | Packet.ChatCommand msg ->
      let* () =
        client.broadcast (Packet.ChatEvent { sender_id = client.id; message = msg }) client.id
      in
      let* () =
        Packet.send
          client.oc
          (Packet.ChatCommandResponse (true, "Message broadcasted successfully"))
      in
      Lwt.return (InGame { player; user_id; player_entity_id })
    | _ -> raise (Invalid_argument "Received non-chat packet in handle_chat_command")
  ;;
```

Then wire it into the main `handle_packet` match:

```ocaml
| Packet.ChatCommand _ -> handle_chat_command client player user_id player_entity_id packet
```

### Summary – Files & Order of Work

| Order | File                           | What you do                                      | Required for |
|-------|--------------------------------|--------------------------------------------------|--------------|
| 1     | `packets.ml`                   | Add Command / Response / Event variants          | every feature |
| 2     | `entities.ml` (maybe)          | Add field(s) to `player` record                  | shared state |
| 3     | `queries.ml`                   | Add focused SQL + Caqti sig                      | persistence  |
| 4     | `repository.ml`                | Wrap query, unwrap errors, return clean result   | persistence  |
| 5     | `client.ml`                    | Add handler, orchestrate DB + packets + state    | every feature |

### Core Design Rules (non-negotiable)

1. **Repository is the hard DB boundary** — never touch Caqti / SQL outside it.
2. **No raw tuples outside `repository.ml`** — always build records.
3. **client.ml is orchestration only** — should read like a story, not a query script.
4. **Handle all three cases** — `Ok value`, `Ok None`, `Error msg`.
5. **On DB error** → send `UnexpectedServerError`, keep the loop alive.
6. **Separate network identity (`client.id`) from persistence identity (`entity_id` / `user_id`)**.
7. **Prefer many small queries** over large ones that try to do everything.

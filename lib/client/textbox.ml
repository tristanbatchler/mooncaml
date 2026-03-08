let empty_edit = Types.{ text = ""; cursor = 0 }

let edit_insert (ed : Types.edit_line) ch =
  let c = ed.cursor in
  let before = String.sub ed.text 0 c in
  let after = String.sub ed.text c (String.length ed.text - c) in
  Types.{ text = before ^ String.make 1 (Char.chr ch) ^ after; cursor = c + 1 }
;;

let edit_backspace (ed : Types.edit_line) =
  let c = ed.cursor in
  if c > 0
  then (
    let before = String.sub ed.text 0 (c - 1) in
    let after = String.sub ed.text c (String.length ed.text - c) in
    Types.{ text = before ^ after; cursor = c - 1 })
  else ed
;;

let edit_delete (ed : Types.edit_line) =
  let c = ed.cursor in
  let len = String.length ed.text in
  if c < len
  then (
    let before = String.sub ed.text 0 c in
    let after = String.sub ed.text (c + 1) (len - c - 1) in
    Types.{ ed with text = before ^ after })
  else ed
;;

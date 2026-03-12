#!/usr/bin/env bash


# Output file
outputFile="ProjectSummary.md"


# Argument flags
no_server=false
no_client=false
no_bmp2ml=false

# Parse command line arguments
for arg in "$@"; do
    case $arg in
        --no-server)
            no_server=true
            ;;
        --no-client)
            no_client=true
            ;;
        --no-bmp2ml)
            no_bmp2ml=true
            ;;
    esac
done

# Write header (overwrite file)
echo -e "# Project Summary\n" > "$outputFile"

# Extensions regex (same filtering behavior as PowerShell)
# Matches files with specific extensions OR files with no extension (LICENSE, PKGBUILD, etc)
regex='\.(cpp|h|hpp|c|cs|py|js|ts|java|go|rs|nim|sh|ps1|gd|jsonc|json|pyi|md|ml|sql)$|^[^.]+$'

# Process each git-tracked file
git ls-files | grep -E "$regex" | while IFS= read -r file; do
    # Skip this script (if named summarize.sh) and output file
    if [[ "$file" == "summarize.sh" || "$file" == "$outputFile" ]]; then
        continue
    fi

    # Omit requested files and directories
    case "$file" in
        .vscode/*|test/*|README.md|LICENSE|dune-project|.gitignore|.ocamlformat|shared/maps.ml|*/shared/maps.ml|lib/server/maps/*.ml)
            continue
            ;;
    esac

    # Exclude server files if --no-server
    if $no_server; then
        case "$file" in
            lib/server/*|bin/server_app.ml)
                continue
                ;;
        esac
    fi

    # Exclude client files if --no-client
    if $no_client; then
        case "$file" in
            lib/client/*|bin/client_app.ml)
                continue
                ;;
        esac
    fi

    # Exclude bmp2ml file if --no-bmp2ml
    if $no_bmp2ml; then
        case "$file" in
            bin/bmp2ml.ml)
                continue
                ;;
        esac
    fi

    # Extract lowercase extension
    ext=".${file##*.}"
    ext="${ext,,}"
    
    # If no extension, use filename as identifier
    if [[ "$ext" == ".$file" ]]; then
        ext=""
    fi

    # Map extension to markdown language
    case "$file" in
        LICENSE)      lang="text" ;;
        PKGBUILD|*.PKGBUILD) lang="bash" ;;
        Makefile)     lang="makefile" ;;
        *)
            case "$ext" in
                .cpp|.h|.hpp) lang="cpp" ;;
                .c)           lang="c" ;;
                .cs)          lang="csharp" ;;
                .py)          lang="python" ;;
                .js)          lang="javascript" ;;
                .ts)          lang="typescript" ;;
                .java)        lang="java" ;;
                .go)          lang="go" ;;
                .rs)          lang="rust" ;;
                .nim)         lang="nim" ;;
                .sh)          lang="bash" ;;
                .ps1)         lang="powershell" ;;
                .gd)          lang="gdscript" ;;
                .jsonc)       lang="jsonc" ;;
                .json)        lang="json" ;;
                .pyi)         lang="python" ;;
                .ml)          lang="ocaml" ;;
                .sql)         lang="sql" ;;
                *)            lang="" ;;
            esac
            ;;
    esac

    {
        echo
        echo "## $file"
        echo
        # For markdown, we use --- and --- instead of ``` to avoid issues with nested code blocks
        if [[ "$lang" == "markdown" ]]; then
            echo "---"
        else
            echo "\`\`\`$lang"
        fi
        cat "$file"
        echo  # Ensure newline after file content
        if [[ "$lang" == "markdown" ]]; then
            echo "---"
        else
            echo "\`\`\`"
        fi
    } >> "$outputFile"

done
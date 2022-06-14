Param(
    [Parameter(Position=0, Mandatory)]
    [string]$program,
    [Parameter(ValueFromRemainingArguments)]
    [string[]]$params
)

cargo run --manifest-path ./assembler/Cargo.toml -- -i "$program" -o program.bin --sourcemap program.map

if ($LASTEXITCODE -eq 0) {
    cargo run --manifest-path ./vm/Cargo.toml -- program.bin --sourcemap program.map $params
}
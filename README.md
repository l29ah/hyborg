# Hyborg

**WIP** parallel borgbackup-compatible backup tool written in Haskell.

## Usage

### Cabal

Install:
```sh
cabal install
```

Or run in-place:
```sh
cabal run hyborg:hyborg -- --help
```

### Nix

```sh
nix-shell --run 'hyborg --help'
```

## Known limitations

* can only back up hardlinks in borg-2.0 format, the earlier borg versions extract hardlinks as separate files
* no proper encryption yet
* no XATTRs and POSIX ACLs backed up yet

## License

[GNU AGPLv3](LICENSE)

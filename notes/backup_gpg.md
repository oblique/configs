### Backup

```
gpg --armor --export > public-keys.asc
gpg --armor --export-secret-keys > private-keys.asc
gpg --export-ownertrust > ownertrust.asc
```

If your `.gnupg` directory is located in another path, you can use `--homedir /path/to/.gnupg`.

### Restore

```
gpg --import public-keys.asc
gpg --import private-keys.asc
gpg --import-ownertrust ownertrust.asc
```

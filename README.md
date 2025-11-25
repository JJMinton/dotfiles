# dotfiles
Configuration files


### Rebuild
```
sudo nixos-rebuild switch -I nixos-config=/home/jeremy/repos/dotfiles/nixos/configuration.nix --upgrade-all
```

### Nixos clean-up
To garbage collect unused packages and old generations in NixOS, use the following commands:

1. Remove unused packages:
```
sudo nix-collect-garbage -d && nix-collect-garbage -d
```

2. Delete old generations for all users (optional):
```
sudo nix-env --delete-generations old
```
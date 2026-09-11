# dotfiles
Configuration files


### Rebuild
```
sudo nixos-rebuild switch --flake /home/jeremy/repos/dotfiles#desktop
```

Update flake inputs with `nix flake update`, then rebuild. The generated `flake.lock` pins the Nixpkgs, Home Manager, NUR, and nixos-hardware revisions.

To test without activating the system:
```
sudo nixos-rebuild dry-build --flake /home/jeremy/repos/dotfiles#nixos
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
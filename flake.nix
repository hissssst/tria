{
  description = "Tria: optimizing Elixir compiler and infrastructure";
  outputs = { self, nixpkgs, ... }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {
      devShells.x86_64-linux.default = import ./shell.nix { inherit pkgs; };
    };
}

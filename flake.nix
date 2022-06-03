{
  description = "A calculator program/website";

  outputs = { self, nixpkgs }:
    let
      systems =
        [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);

      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        });
    in rec {
      overlay = final: prev: {
        kalker = final.rustPlatform.buildRustPackage {
          pname = "kalker";
          version = "unstable";
          description = "A CLI calculator";

          src = self;

          nativeBuildInputs = with final; [ gcc ];

          outputs = [ "out" "lib" ];

          postInstall = ''
            moveToOutput "lib" "$lib"
          '';

          cargoLock = { lockFile = self + "/Cargo.lock"; };

          buildInputs = with final; [ gmp mpfr libmpc ];

          CARGO_FEATURE_USE_SYSTEM_LIBS = "1";
        };
      };

      packages =
        forAllSystems (system: { inherit (nixpkgsFor.${system}) kalker; });

      defaultPackage = forAllSystems (system: self.packages.${system}.kalker);

      apps = forAllSystems (system: {
        kalker = {
          type = "app";
          program = "${self.packages.${system}.kalker}/bin/kalker";
        };
      });

      defaultApp = forAllSystems (system: self.apps.${system}.kalker);

      devShell = forAllSystems (system:
        nixpkgs.legacyPackages.${system}.mkShell {
          inputsFrom = builtins.attrValues (packages.${system});
        });
    };
}

{
  description = "A very basic flake";

  outputs = { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        #"aarch64-darwin" # currently not building due to gmp
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);

      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        });
    in {
      overlay = final: prev: {
        kalker = final.rustPlatform.buildRustPackage {
          pname = "kalker";
          version = "unstable";

          src = self;

          nativeBuildInputs = with final; [ m4 ];

          outputs = [ "out" "lib" ];

          postInstall = ''
            moveToOutput "lib" "$lib"
          '';

          cargoLock = { lockFile = self + "/Cargo.lock"; };

        }
        # FIXME: fix aarch64-darwin builds
        /* // (if (final.stdenv.isDarwin && final.stdenv.isAarch64) then {
             CARGO_FEATURE_USE_SYSTEM_LIBS = "1";
             RUST_BACKTRACE = "1";
             buildInputs = with final; [ gmp mpfr libmpc ];
           } else
             { })
        */
        ;
      };

      packages = forAllSystems (system: nixpkgsFor.${system});

      defaultPackage = forAllSystems (system: self.packages.${system}.kalker);

    };
}

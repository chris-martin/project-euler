with import <nixpkgs> {};
with goPackages;

let

  prime = buildFromGitHub {
    rev = "v2.1";
    owner = "kavehmz";
    repo = "prime";
    sha256 = "0kmy9cv1l2hd7187l2jfdzhf0k0qhlq1kxh72fzp7m9s7l37pfc1";
  };

in

  buildGoPackage {
    name = "project-euler";
    version = "0.0.1";
    goPackagePath = "github.com/chris-martin/project-euler";
    buildInputs = [ prime ];
    src = ./.;

    # https://github.com/NixOS/nixpkgs/pull/15490
    shellHook = ''
      d=$(mktemp -d "--suffix=-$name")
      mkdir -p "$d/src/$(dirname "$goPackagePath")"
      ln -s "$src" "$d/src/$goPackagePath"
      export GOPATH="$d:$GOPATH"
    '';
  }

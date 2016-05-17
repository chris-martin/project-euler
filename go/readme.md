Fast tests:

```bash
env EULER_PATH=$PWD/.. nix-shell --run \
  'go test github.com/chris-martin/project-euler/tests'
```

A specific problem test:

```bash
env EULER_PATH=$PWD/.. PROBLEM=7 nix-shell --run \
  'go test github.com/chris-martin/project-euler/tests'
```

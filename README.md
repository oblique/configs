## Init

```sh
mkdir -p ~/git
git clone --recursive https://github.com/oblique/configs ~/git/configs
chezmoi apply -S ~/git/configs
```

## Polybar

Install my custom modules with:

```sh
cargo install -f --git https://github.com/oblique/polybar-custom-modules
```

# MINI-TEST

A minimal test library, born for `Fortran`.

## Getting started

### Get the code

```sh
git clone https://github.com/zoziha/mini-test.git
cd mini-test
```

### Build with [fortran-lang/fpm](https://github.com/fortran-lang/fpm)

Fortran Package Manager (fpm) is a great package manager and build system for Fortran.   
You can build using provided `fpm.toml`:
```sh
fpm build
```

To use `mini-test` for your `fpm` project tests, add the following to your `fpm.toml` file:
```toml
[dev-dependencies]
mini-test = { git="https://github.com/zoziha/mini-test.git" }
```

## API-Doc

```fortran
public :: check, is_close
```

Some examples are prepared in the `./example` folder, and you can use `fpm` to run them:
```sh
fpm run --example --list
fpm run --example <demo_name, see `fpm.toml` or list>
```

## Links

[fortran-lang/stdlib](https://github.com/fortran-lang/stdlib)
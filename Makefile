# Fortran `mini-test` Makefile, only for developement.
FYPPFLAGS=

export FYPPFLAGS

.PHONY: dev deploy clean

dev:
	$(MAKE) -f Makefile --directory=meta-src

deploy:
	mkdir -p mt-fpm/src
	cp src/*.f90 mt-fpm/src/
	cp LICENSE mt-fpm/
	cp fpm.toml mt-fpm/

clean:
	$(MAKE) -f Makefile --directory=meta-src clean
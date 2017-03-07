setup:
	stack setup

build:
	stack build

run-aeson-example:
	stack exec aeson-example-exe

run-lens-example:
	stack exec lens-example-exe

run-servant-example:
	stack exec servant-example-exe

run-tisch-example:
	stack exec tisch-example-exe

run-trace-example:
	stack exec tisch-example-exe

test-servant-example:
	curl http://127.0.0.1:8080/users

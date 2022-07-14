## Build binary and docker images
build:
  # Note how we tell stack to copy the static executable to a directory called
  # bin/
  # This is explicitly copied into the docker image
	@stack build --copy-bins --local-bin-path ./bin

  # Docker compose looks for docker-compose.yml
  # Which in turn looks for Dockerfile
	@sudo docker compose build

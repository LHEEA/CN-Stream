all: docker

docker_image: Dockerfile
	docker build . -t cnstream_build

docker: docker_image
	docker run --rm -u $(shell id -u ):$(shell id -g ) \
		-v $(shell pwd):/opt/share \
		-w /opt/share \
		cnstream_build /bin/bash -c \
           "cd /opt/share &&\
            mkdir -p build_docker &&\
            cd build_docker &&\
            cmake .. && \
            make && \
            make test ARGS=\"-V\""
clean:
	rm -rf build_docker

.PHONY: docker docker_image clean

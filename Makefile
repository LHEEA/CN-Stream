all: docker

docker_image: Dockerfile
	@echo "Build docker image cnstream_build"
	docker build . -t cnstream_build

docker: docker_image
	@echo "Compile CN-stream with docker image cnstream_build"
	docker run --rm -u $(shell id -u ):$(shell id -g ) \
	    -v $(shell pwd):/opt/share \
	    -w /opt/share \
	    cnstream_build /bin/bash -c \
           "cd /opt/share && \
            mkdir -p build_docker && \
            cd build_docker && \
            cmake .. && \
            make && \
            make test ARGS=\"-V\" && \
            make package"

docker_test: docker_image
	docker run --rm -u $(shell id -u ):$(shell id -g ) \
	    -v $(shell pwd):/opt/share \
	    -w /opt/share \
	    cnstream_build /bin/bash -c \
           "cd /opt/share && \
            mkdir -p build_docker && \
            cd build_docker && \
            make test"


clean:
	rm -rf build build_docker lib main mainCNS prog

.PHONY: docker docker_image clean

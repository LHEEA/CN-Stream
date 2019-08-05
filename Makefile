# This Makefile allows to compile CN-stream with docker.
# A single 'make' command will
#
#  - build the required docker image
#  - configure project with CMake
#  - compile project
#  - run test
#  - create an installation package

all: docker

docker_image: Dockerfile
	@echo "Build docker image cnstream_build"
	docker build . -t cnstream_build

docker: docker_image
	@echo "Compile, test and package CN-stream with docker image cnstream_build"
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

docker_make: docker_image
	@echo "Compile CN-stream with docker image cnstream_build"
	docker run --rm -u $(shell id -u ):$(shell id -g ) \
	    -v $(shell pwd):/opt/share \
	    -w /opt/share \
	    cnstream_build /bin/bash -c \
           "cd /opt/share && \
            mkdir -p build_docker && \
            cd build_docker && \
            cmake .. && \
            make"

docker_animate: docker_image
	@echo "Create an animation"
	docker run --rm -u $(shell id -u ):$(shell id -g ) \
        -v $(shell pwd):/opt/share \
        -w /opt/share \
        cnstream_build /bin/bash -c \
           "cd /opt/share && \
            for timestep in 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2 1.3 1.4 1.5; \
            do \
            echo \$${timestep}; \
            cp input/CN_Stream_input.dict input/CN_Stream_input.dict.ORI; \
            sed -i \"s/time 2.5/time \$${timestep}/g\" /opt/share/input/CN_Stream_input.dict; \
            cat /opt/share/input/CN_Stream_input.dict; \
            ./mainCNS; \
            mv input/CN_Stream_input.dict.ORI input/CN_Stream_input.dict; \
            mv output/VP_card_fitted.dat output/VP_card_fitted_\$${timestep}.dat; \
            done"

clean:
	rm -rf build build_docker lib main mainCNS prog

.PHONY: docker docker_image clean

build:
	uv build

install: build
	pipx install dist/*.whl --force

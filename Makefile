all: frontend backend


frontend:
	cd frontend && elm make src/Main.elm


backend:
	cd backend && ./build.sh

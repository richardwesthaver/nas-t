MODE?=release
OUT_DIR?=/mnt/w/nas-t.net
IP?=::1
PORT?=8082
R?=cargo
all:build
.PHONY:clean fmt client server
# TODO 2023-08-01: replace with organ.who
# pub:content.org pub.el;emacs --batch --load pub.el --eval (nas-t-web-publish)
test:;
bc:;cd client && npm ci && CARGO_TARGET_DIR=../target-trunk trunk build --$(MODE)
bs:;cd server && cargo build --release
build:bc bs;
client:;cd client; && trunk serve --addr $(IP) --port $(PORT)
server:;cd server;$(R) run -- --port 13008
deploy:build;cp -rf client/dist/* $(OUT_DIR)
clean:;rm -rf target-trunk client/dist target Cargo.lock client/node_modules
fmt:;$(R) fmt

IP?=::1
PORT?=13009
R?=cargo
all:client server
.PHONY:clean fmt client server
# TODO 2023-08-01: replace with organ.who
# pub:content.org pub.el;emacs --batch --load pub.el --eval (nas-t-web-publish)
test:;

client:;cd client;CARGO_TARGET_DIR=../target-trunk trunk serve --address $(IP) --port $(PORT)
server:;cd server;cargo watch -- cargo run -- --port 13008

clean:;rm -rf target-trunk dist target Cargo.lock
fmt:;$(R) fmt

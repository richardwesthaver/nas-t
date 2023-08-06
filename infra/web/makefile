MODE?=release
ifeq ($(MODE),release)
  args=--release
endif
OUT_DIR?=/mnt/w/nas-t.net
IP?=::1
PORT?=8082
R?=cargo
contents=$(filter-out client/index.html, $(wildcard client/*.html))
styles=../style/images/favicon.ico ../style/images/logo.svg
all:build
.PHONY:clean fmt client server
content:content.el content.org;emacsclient -n -e '(load-file "content.el")' -e '(nas-t-content-export)'
style:$(styles);cp $^ client
js:;cd client && npm ci
bc:content style js;cd client && npm ci && CARGO_TARGET_DIR=target-trunk trunk build $(args)
bs:;cd server && cargo build $(args)
build:bc bs;
client:;cd client; && trunk serve --addr $(IP) --port $(PORT)
server:;cd server;$(R) run -- --port 13008
test:;
deploy:build;cp -rf client/dist/* $(OUT_DIR)
clean:;rm -rf target-trunk client/dist target Cargo.lock client/node_modules $(contents)
fmt:;$(R) fmt

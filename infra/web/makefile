IP?=::1
PORT?=13009
.PHONY:test ui dev-api clean
test:;
ui:;cd ui;CARGO_TARGET_DIR=../target-trunk trunk serve --address $(IP) --port $(PORT)
dev-api:;cd server; cargo watch -- cargo run -- --port 13008
clean:;rm -rf target-trunk dist trunk Cargo.lock

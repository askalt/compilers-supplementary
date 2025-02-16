
all: test

.PHONY: cargo_fmt
cargo_fmt:
	cargo fmt --all --check

.PHONY: clippy
clippy:
	cargo clippy -- --deny warnings

# Run rust tests.
.PHONY: cargo_test
cargo_test:
	cargo test --all

# Run haskell tests.
.PHONY: hs_test
hs_test:
	(cd hs && cabal run)

# Run all tests.
.PHONY: test
test: cargo_test hs_test

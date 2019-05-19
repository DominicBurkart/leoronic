FROM rust:1.34
ADD ../leoronic/* leoronic
ADD * /
RUN erlc +native leoronic/*.erl
RUN cargo build --release
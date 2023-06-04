FROM fpco/stack-build:lts-20.23 AS builder
COPY . /app
WORKDIR /app
RUN stack build
RUN cp -r $(stack path --local-install-root)/bin /app/bin

FROM haskell:9.2.7-slim
WORKDIR /app
RUN apt update && apt install -y libncurses5
COPY --from=builder /app/bin ./bin
CMD ["./bin/better-reads-web"]

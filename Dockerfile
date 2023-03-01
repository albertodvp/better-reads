FROM haskell:9.2
COPY . /app

WORKDIR /app

RUN cabal update && cabal build

CMD ["cabal", "run", "better-reads-web"]

FROM fpco/stack-build:lts

COPY . /app

WORKDIR /app

RUN stack build

CMD ["stack" "run" "--", "better-reads-web"]

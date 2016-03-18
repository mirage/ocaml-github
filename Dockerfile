FROM ocaml/opam
RUN opam pin add -n github --dev
RUN opam depext -ui github
RUN opam install -j 2 -y -v github

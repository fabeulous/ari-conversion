# trs conversion

This repository is the home of three tools:

 * trs-converion: can converts between the ARI and COPS formats
 * format-type: extracts the type of the problem from the ARI format
 * ari-syntax-checker: checks if a ARI problem is syntactically correct

## Building the tools

To build the tool using `stack` just run the command

    stack build trs-conversion

## Running the tools

After building you can run the respective tool with:

    stack exec trs-conversion -- --from FORMAT --to FORMAT FILE

    stack exec format-type -- FILE

    stack exec ari-syntax-checker -- FILE

where FORMAT can be "COPS" or "ARI".




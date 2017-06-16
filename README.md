# kappa-test

To compile, run `make`.

You can see all options by typing `./KaStats --help`.



Examples :

`./KaStats -r 'a.c' trace.json` will analyze the rule _a.c_ in _trace.json_ and write output in a file named _trace.log_.

`./KaStats -r 'a.c' --all -o output.txt trace.json` will analyse the rule _a.c_ in _trace.json_ and write output in a file named _output.txt_. It will show stats for every site, even sites that are tested by the rule.



**Note :** for now, the trace file is entirely loaded into memory, so giving a big trace file to KaStats will result in an _Out of Memory_ error.

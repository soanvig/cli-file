# TODO

- [ ] Calling command without parameter should show interactive selection of command (or optionally with --interactive flag?)
- [ ] Add CI with build and release
- [x] Add support for longer than one-word argument default values
- [ ] Add support for escaping double-quote in argument default value
- [ ] Add support for naming user params to supply params in different order
- [ ] Add support for qutting process with Ctrl+C (or any similar)
- [ ] Minimize `try` backtracking scope: http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/
- [ ] Improve parsing rules to improve parsing errors (for example empty command name without a space returns strange error)
- [x] Add support for more characters (e.g. *) in argument default values
- [x] Parse whole file, instead of lines per line (this will allow correct parsing error report on line details, and other features)
- [x] README documentation
- [x] Add support for selecting command file location
- [x] Add --version and --help commands
- [x] Add CI with tests
- [x] Support for empty lines in command file
- [x] Add support for comments

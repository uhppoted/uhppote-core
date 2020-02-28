## v0.5.1x

** IN PROGRESS **

1. Restructure for Go modules and git submodules

## TODO

### uhppote
- [ ] concurrent requests
- [ ] update tests with Errorf + return to Fatalf
- [ ] commonalise ACL
- [ ] commonalise configuration
- [ ] make types consistent across API
- [ ] Genericize message unit tests
- [ ] Convert to 1.13 error handling
- [ ] Rework UHPPOTE response messages to use factory
- [ ] Fix golint errors
- [ ] Invert conf Unmarshal so that it iterates struct rather than file (simplifies e.g. DeviceMap)
- [ ] Rework plist encoder/decoder to be only for launchd (and remove 'parse' from daemonize/undaemonize)
- [ ] Unify event buffer operations

### Documentation

- [ ] TeX protocol description
- [ ] ASN.1 protocol specification
- [ ] godoc
- [ ] build documentation

### Other

1.  Rework uhppote to use bidirectional channel to serialize requests
2.  Look into UDP multicast
3.  Look into ARP for set-address
4.  github project page
5.  Verify fields in listen events/status replies against SDK:
    - battery status can be (at least) 0x00, 0x01 and 0x04
6.  PDL + go generate
    - [lipPDL](http://nmedit.sourceforge.net/subprojects/libpdl.html)
    - [Diva](http://www.diva-portal.org/smash/get/diva2:407713/FULLTEXT01.pdf)
    - [PADS/ML](https://pads.cs.tufts.edu/papers/tfp07.pdf)
    - [PADS](https://www.cs.princeton.edu/~dpw/papers/700popl06.pdf)
    - [DataScript](https://www.researchgate.net/publication/221108676_DataScript-_A_Specification_and_Scripting_Language_for_Binary_Data)
    - [PADS/ML](https://www.cs.princeton.edu/~dpw/papers/padsml06.pdf)
    - [PADS Project](http://www.padsproj.org/)
    - [Mozilla IPDL](https://developer.mozilla.org/en-US/docs/Mozilla/IPDL/Tutorial)
    - [PDL: Failure Semanics](https://www.researchgate.net/publication/2784726_A_Protocol_Description_Language_for_Customizing_Failure_Semantics)
    - https://en.wikipedia.org/wiki/Abstract_Syntax_Notation_One
7.  [Ballerina](https://ballerina.io)
8.  [Eclipse Kura](https://www.eclipse.org/kura)
9.  [Klee](https://klee.github.io)
10. [AsyncAPI](https://www.asyncapi.coms)
     - https://modeling-languages.com/asyncapi-modeling-editor-code-generator
11. go-fuzz

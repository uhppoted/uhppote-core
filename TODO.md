## v0.7.x

### IN PROGRESS

- Shared-lib/DLL
  - [x] `help` for examples
        - [x] C
        - [x] C++
        - [x] C#
        - [x] Python
        - [x] CCL

  - [x] `get-card-by-index`
        - [x] C
        - [x] C++
        - [x] C#
        - [x] Python
        - [x] CCL

  - [ ] `put-card`
  - [ ] `delete-card`
  - [ ] `delete-cards`
  - [ ] `get-event-index`
  - [ ] `set-event-index`
  - [ ] `get-event`
  - [ ] `record-special-events`
  - [ ] `open-door`
  - [ ] `get-time-profile`
  - [ ] `set-time-profile`
  - [ ] `clear-time-profiles`
  - [ ] `add-task`
  - [ ] `refresh-tasklist`
  - [ ] `clear-task-list`

  - [ ] Tests usage
  - [ ] Extend examples to use command line args
        - [ ] Brief usage() list commands
        - [ ] Python usage()
  - [ ] C++ 
        - std::invoke
  - [ ] python: 
        - constants for ControlMode
        - https://martinheinz.dev/blog/70
  - [ ] CCL
        - constants + strings for ControlMode
        - DOLIST
        - simplify main loop with `thereis` (https://gigamonkeys.com/book/loop-for-black-belts.html)
        - test failure format
```
set-address       ok
get-status        ok
get-time:    incorrect date/time - expected:2022-01-02 12:34:56, got:2022-01-02 12:34:56
```

  
  - [ ] lint
  - (?) Cross-compile
        - https://github.com/elastic/golang-crossbuild

- [ ] Check UHPPOTE encoding for revised Date implementation
      - should return Date{} for 0000...
      - should return DateTime{} for 0000...
- [ ] Rework any remaining Date/DateTime pointers to rather use IsZero/IsValid

## TODO

- [ ] Replace messy UDP broadcast wait implementation with WaitGroup
- [ ] Fix DateTime Unmarshal to set nanos etc to 0
- [ ] (?)EventNotFoundError
- [ ] (?)EventOverwrittenError

- [ ] Set first card
- [ ] See if there's anything worth taking from CloudFlare blog post on UDP:
      https://blog.cloudflare.com/everything-you-ever-wanted-to-know-about-udp-sockets-but-were-afraid-to-ask-part-1/

- [ ] Implement separate Get..All functions (??)
- [ ] Add https://odi.ch/weblog/posting.php?posting=731 to cookbook
- [ ] (?) Rename types.Time to types.DeviceTime
- [ ] (?) Rename types.SystemTime to types.Time
- [ ] (?) Replace types.SystemDate with types.Date

- [ ] Look at https://github.com/mitchellh/cli and https://github.com/mitchellh/go-glint

### uhppote
- [ ] make types consistent across API
- [ ] Genericize message unit tests
- [ ] Convert to 1.13 error handling
- [ ] Fix golint errors
- [ ] Invert conf Unmarshal so that it iterates struct rather than file (simplifies e.g. DeviceMap)
- [ ] Rework plist encoder/decoder to be only for launchd (and remove 'parse' from daemonize/undaemonize)
- [ ] Unify event buffer operations
- [ ] Test IPConfig utility 'search message'
       ... received 20 bytes from 155.138.158.102:54544
       ...          00000000  24 10 f4 33 02 06 00 00  00 00 00 00 ff ff ff ff
       ...          00000010  00 00 02 00
      
       ... received 20 bytes from 155.138.158.102:54544
       ...          00000000  24 10 b0 31 01 02 00 00  00 00 00 00 ff ff ff ff
       ...          00000010  00 00 01 00
       ...

       ... received 20 bytes from 155.138.158.102:54544
       ...          00000000  24 10 f4 33 02 06 00 00  00 00 00 00 ff ff ff ff
       ...          00000010  00 00 02 00
       ...

       ... received 20 bytes from 155.138.158.102:54544
       ...          00000000  24 10 ba 37 03 08 00 00  00 00 00 00 ff ff ff ff
       ...          00000010  00 00 03 00
       ...

       ... received 20 bytes from 155.138.158.102:54544
       ...          00000000  24 10 b0 32 05 02 00 00  00 00 00 00 ff ff ff ff
       ...          00000010  00 00 05 00
       ...

       ... received 24 bytes from 155.138.158.102:54544
       ...          00000000  20 10 b1 f9 06 02 00 00  00 00 00 00 ff ff ff ff
       ...          00000010  00 00 01 00 ff ff ff ff
       ...

       ... received 24 bytes from 155.138.158.102:54544
       ...          00000000  20 10 7d 2b 07 06 00 00  00 00 00 00 ff ff ff ff
       ...          00000010  00 00 02 00 ff ff ff ff
       ...

       ... received 24 bytes from 155.138.158.102:54544
       ...          00000000  20 10 42 ed 08 08 00 00  00 00 00 00 ff ff ff ff
       ...          00000010  00 00 03 00 ff ff ff ff
       ...

       ... received 24 bytes from 155.138.158.102:54544
       ...          00000000  20 10 e4 83 09 02 00 00  00 00 00 00 ff ff ff ff  | ...............|
       ...          00000010  00 00 05 00 ff ff ff ff                           |........|
       ...

- [ ] bad udp cksum
      tcpdump: listening on en3, link-type EN10MB (Ethernet), capture size 262144 bytes
      14:08:25.074807 IP (tos 0x0, ttl 64, id 55147, offset 0, flags [none], proto UDP (17), length 92, bad cksum 0 (->9bf4)!)
      192.168.66.225.60267 > 192.168.66.255.60000: [bad udp cksum 0x078b -> 0x0acc!] UDP, length 64
      0x0000:  4500 005c d76b 0000 4011 0000 c0a8 42e1  E..\.k..@.....B.
      0x0010:  c0a8 42ff eb6b ea60 0048 078b 1794 0000  ..B..k.`.H......
      0x0020:  0000 0000 0000 0000 0000 0000 0000 0000  ................
      0x0030:  0000 0000 0000 0000 0000 0000 0000 0000  ................
      0x0040:  0000 0000 0000 0000 0000 0000 0000 0000  ................
      0x0050:  0000 0000 0000 0000 0000 0000            ............

### Documentation

- [ ] TeX protocol description
- [ ] ASN.1 protocol specification
- [ ] godoc
- [ ] build documentation

### Other

1.  Rework uhppote to use bidirectional channel to serialize requests
2.  Look into ARP for set-address
3.  Verify fields in listen events/status replies against SDK:
    - battery status can be (at least) 0x00, 0x01 and 0x04
4.  PDL + go generate
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
5.  [Ballerina](https://ballerina.io)
6.  [Eclipse Kura](https://www.eclipse.org/kura)
7.  [Klee](https://klee.github.io)
8 . [AsyncAPI](https://www.asyncapi.coms)
     - https://modeling-languages.com/asyncapi-modeling-editor-code-generator
9.  go-fuzz

10. cgo: explicitly set alignment `__attribute__((align(8))) complex float x;`
    -  https://github.com/golang/go/wiki/cgo#struct-alignment-issues
11. [Transpilers](http://anachronauts.club/~voidstar/log/2022-03-24-openapi-for-binfmt.gmi)

### Notes
1.  https://stevelosh.com/blog/2021/03/small-common-lisp-cli-programs/


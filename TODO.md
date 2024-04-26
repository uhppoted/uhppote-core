# TODO

- [ ] TCP/IP protocol (cf. https://github.com/uhppoted/uhppote-core/issues/17)
      - [x] Verify TCP with real controller
      - [x] TCP 'driver'
      - [x] See if controller sends events to TCP listener
      - [ ] Unify send/sendto implementations
      - [ ] Rework bind address as netip.AddrPort
      - [ ] Rework broadcast address as netip.AddrPort
      - [ ] Rework listen address as netip.AddrPort
      - [ ] Rework address as *netip.AddrPort
            - [x] uhppote-core
            - [x] Update uhppoted-lib::conf
            - [x] uhppote-cli
            - [x] uhppoted-lib
            - [ ] Replace pointer with zero value
            - [ ] // FIXME rework u.BroadCastAddr as netip.AddrPort
      - [ ] Add protocol field to UHPPOTE controller struct
            - [x] uhppote-core
            - [x] Update uhppoted-lib::conf
            - [ ] Update throughout
                  - [x] uhppote-cli
                  - [x] uhppoted-lib
                  - [x] uhppoted-rest
                  - [x] uhppoted-mqtt
                  - [ ] uhppoted-httpd
                  - [x] uhppoted-app-db
                  - [ ] uhppoted-app-s3
                  - [ ] uhppoted-app-sheets
                  - [ ] uhppoted-app-wild-apricot
                  - [ ] uhppoted-codegen
                  - [ ] uhppoted-dll
                  - [ ] uhppoted-tunnel
      - [ ] Add TCP to simulator
      - [ ] CHANGELOG
      - [ ] README

- [ ] Rework UDP to use connected sockets
      - https://github.com/uhppoted/uhppote-simulator/issues/9
      - // FIXME don't wait for timeout if reply is valid

- [ ] Rework any remaining Date/DateTime pointers to rather use IsZero/IsValid
- [ ] `put-card`: error on bad card numbers (0,0xffffffff,0x00ffffff)

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


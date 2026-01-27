// Copyright 2023 uhppoted@twyst.co.za. All rights reserved.
// Use of this source code is governed by an MIT-style license
// that can be found in the LICENSE file.

/*
Package uhppote-core implements the public API for UHPPOTE TCP/IP Wiegand-26
access controllers.

The API comprises an RPC-like protocol implemented using
64 byte UDP messages between the controller and the application.

# Structure

The library consists of the following packages:
  - [uhppote-core/uhppote] which wraps the UDP request/response packet handling for the
    communication with the the access controller.
  - [uhppote-core/messages] which defines the UDP message structures used for the RPC-like
    API.
  - [uhppote-core/types] which defines the data types used in the `messages` package.
  - [uhppote-core/encoding/UTO311-L0x] which implements message marshalling and unmarshalling
    to and from the 64 byte packets exchangeed with the access controller.

# Deprecation Notice

Note that as of v0.9.0, uhppote-core is flagged for internal use only as the functionality is migrated
to uhppoted-lib. Please consider using:

[uhppoted-lib-go]: https://github.com/uhppoted/uhppoted-lib-go
*/
package core

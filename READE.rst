===========================
 Haskell smartcard - alpha
===========================

This module contains bindings for the pcsclite library, which allows talking to smartcards and RFID tags.
Please note, that this is a alpha release, so do not expect too much comfort yet.

Building
~~~~~~~~~

Make sure you have pcsclite installed. There are packages for almost every distro out there.
After the checkout::
  
  user$: cd smartcard
  user$: cabal configure
  user$: cabal build

Running
~~~~~~~~

Currently there is a small (rather ugly) testprogram, that sends a *pseudo* APDU to the *ACS ACR 38U-CCID*
to toggle the LED. This is reader specific command and might not work for everybody.

After building run by::
  
  user$: cd dist/build/test
  user$: ./test

now something similar should appear::

  Found readers: ["ACS ACR 38U-CCID 00 00"]
  Connected, Protocol is: T0
  Answer is: [144,2]
  Queriying the status: Right ("ACS ACR 38U-CCID 00 00",[Present,Powered,Negotiable],T0,[59,190,149,0,0,65,3,0,0,0,0,0,0,0,0,0,2,144,0])
  Listing the reader groups: Right ["SCard$DefaultReaders"]


Known problems
~~~~~~~~~~~~~~~

* The interface does still feel very C-ish, so a lot of work has to be done there.
  Moreover it would be nice to have a better representation of the *APDUs*.
* Add bindings for the *SCardGetAttr* and *SCardSetAttr*.

License
~~~~~~~~

For licensing information see LICENSE.

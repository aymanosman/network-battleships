# Network Battleship 

Written for January 2016's [West London Hack Night](http://www.meetup.com/West-London-Hack-Night/).

## Building

You'll need [stack](https://github.com/commercialhaskell/stack). Then call

``` sh
stack build
stack exec battleship
```

Players can connect with `telnet <your-ip> 8000` and send battleship
coordinates like `(1,3)`.

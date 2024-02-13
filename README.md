# wolfram

## Description

This is a simple Cellular Automata implementation in haskell done in a one-day rush.<br>
It is based on the Wolfram's elementary cellular automata.

## Usage

    ./wolfram --rule <rule> (flags)

    Flags:
        --rule <rule>   : The rule to use (30|90|110)
        --start <start> : The initial state of the automata (default:0)
        --lines <lines> : The number of lines to print (default:infinity)
        --window <win>  : The window size (default:80)
        --move <move>   : The offset of the window (default:0)

## Rules

The rules are based on the Wolfram's elementary cellular automata. The rules are:

- 30
- 90
- 110

## Example

    ./wolfram-exe --rule 90 --lines 150 --window 350

## Build
    make

## Author

- [**Maël RABOT**](https://www.linkedin.com/in/mael-rabot/)

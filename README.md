# ytdl

Unfinished personal project

Utility to manage downloads of videos from a set of youtube channels

## TODO

* Get cli opts to work with optional args
* Check cli opts help makes sense
* Put cli options definitions outside Main.hs

## Command line options

ytdl [--channels filename] [--name string] [--tag string] [-s | --simulate]

* --channels specifices the location of the channels file, defaults to channels.json in the current directory
* --name and --tag restrict downloads which have a name or tag that includes the string (case-insensitively)
* --simulate passes that option onto youtube-dl

## Channels file

JSON format. All fields are optional except url.

    [
    {
      url: string,
      format: string,
      name: string,
      tags: [string],
      max: number,
      format: string,
      match: string,
      reject: string,
      disabled: boolean
    }
    ]

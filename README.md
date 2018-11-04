# ytdl

Unfinished personal project

Utility to manage downloads of videos from a set of youtube channels

## TODO

* Allow over-ride of all global parameters. Use .ytdl.json file for config (including output dir)
* Check cli opts help makes sense

## Command line options

ytdl [--channels filename] [--name string] [--tag string] [-s | --simulate]

* --channels specifices the location of the channels file, defaults to ~/.ytdl.json
* --name and --tag restrict downloads which have a name or tag that includes the string (case-insensitively)
* --simulate passes that option onto youtube-dl

## Channels file

JSON format. All fields are optional except url.

    [
    {
      url: string,
      name: string,
      format: string,
      tags: [string],
      max: number,
      match: string,
      reject: string,
      disabled: boolean
    }
    ]

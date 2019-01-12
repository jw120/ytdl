# ytdl

TODO - stylish haskell (install with stack and then run)
TODO - hlint?

Simple script for personal use.

Utility to manage downloads of videos from a set of youtube channels using youtube-dl.
Intended to be run in the folder where the videos are to be downloaded (and the archive file
is located)

## Possible future enhancements

* Allow changing of other parameters
* Tests

## Command line options

ytdl [--channels filename] [--name string] [--tag string] [-s | --simulate]

* --channels specifices the location of the channels file, defaults to ~/.ytdl_channels.json
* --config specifies the location of the config file, defaults to ~/.ytdl_config.json
* --name and --tag restrict downloads which have a name or tag that includes the string (case-insensitively)
* --simulate passes that option onto youtube-dl
* --echo just echoes the commands to the console instead of running them

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

## Config file

JSON format. All fields optional

outputDir: string (diretory to download files to, if missing then current directory)
maxVideos: number (number of videos from each channel, f missing then 10)
defaultFormat: string (default format of video to download, if missing then 18)

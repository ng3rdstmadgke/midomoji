package com.github.ng3rdstmadgke.midomoji;

object OptionParser {
  def parse(args: Array[String]): (String, Map[String, String]) = {
    def _parse(args: List[String], ret: Map[String, String]): (String, Map[String, String]) = {
      args match {
        case "build-dict"      :: rest => ("build-dict"     , buildOptionParser(rest));
        case "build-matrix"    :: rest => ("build-matrix"   , buildOptionParser(rest));
        case "build-config"    :: rest => ("build-config"   , buildOptionParser(rest));
        case "build-pos-info"  :: rest => ("build-pos-info" , buildOptionParser(rest));
        case "build-meta-info" :: rest => ("build-meta-info", buildOptionParser(rest));
        case "check-dict"      :: rest => ("check-dict"     , buildOptionParser(rest));
        case "check-matrix"    :: rest => ("check-matrix"   , buildOptionParser(rest));
        case "debug"           :: rest => ("debug"          , buildOptionParser(rest));
        case "analyze"         :: rest => ("analyze"        , analyzeOptionParser(rest));
        case _                         => ("help"           , Map[String, String]());
      }
    }
    _parse(args.map(_.trim).toList, Map[String, String]());
  }

  def buildOptionParser(args: List[String]): Map[String, String] = {
    def go(args: List[String], ret: Map[String, String]): Map[String, String] = {
      args match {
        case Nil => ret;
        case some :: rest => {
          if (!ret.contains("dict-dir")) {
            go(rest, ret + ("dict-dir" -> some));
          } else {
            go(rest, ret);
          }
        }
      }
    }
    go(args, Map[String, String]());
  }

  def analyzeOptionParser(args: List[String]): Map[String, String] = {
    def go(args: List[String], ret: Map[String, String]): Map[String, String] = {
      args match {
        case Nil => ret;
        case ("-f" | "--format") :: format :: rest => {
           go(rest, ret + ("format" -> format));
        }
        case some :: rest => {
          if (!ret.contains("target")) {
            go(rest, ret + ("target" -> some));
          } else {
            go(rest, ret);
          }
        }
      }
    }
    go(args, Map[String, String]());
  }
}

package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Cd(dirName: String) extends Command {


  override def apply(state: State): State = {
    /*
    cd /something/somethingElse/
    cd a/b/c/

    cd ..
    cd .
    cd a/./.././a/

     */

    // 1. Find the root
    val root = state.root
    val wd = state.wd

    // 2. find the absolute path of the directory I want to cd to
    val absolutePath = {
      if (dirName.startsWith(Directory.SEPARATOR)) dirName
      else if (wd.isRoot) wd.path + dirName
      else wd.path + Directory.SEPARATOR + dirName
    }

    // 3. find the directory to cd to, given the path
    val destinationDir = doFindEntry(root, absolutePath)

    // 4. change the state given the new directory
    if (destinationDir == null || !destinationDir.isDirectory)
      state.setMessage(dirName + ": no such directory")
    else
      State(root, destinationDir.asDirectory)

  }

  def doFindEntry(root: Directory, path: String): DirEntry = {

    @tailrec
    def findEntryHelper(currentDirectory: Directory, path: List[String]): DirEntry = {
      if (path.isEmpty || path.head.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.findEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if (nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory, path.tail)
      }
    }

    // 1. tokens
    val tokens = path.substring(1).split(Directory.SEPARATOR).toList

    // 2. navigate to the correct entry
    findEntryHelper(root, tokens)

  }
}

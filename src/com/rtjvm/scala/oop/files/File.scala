package com.rtjvm.scala.oop.files

class File(override val parentPath: String, override val name: String, val contents: String)
  extends DirEntry(parentPath, name) {
  def setContents(newContents: String): File =
    new File(parentPath, name, newContents)

  def appendContents(newContents: String): File =
    setContents(contents + "\n" + newContents)


  override def asDirectory: Directory =
    throw new FilesystemException("File cannot be converted to a directory")

  override def getType: String = "File"

  override def asFile: File =
    this

  override def isDirectory: Boolean = false

  override def isFile: Boolean = true
}

object File {

  def empty(parentPath: String, name: String): File =
    new File(parentPath, name, "")
}
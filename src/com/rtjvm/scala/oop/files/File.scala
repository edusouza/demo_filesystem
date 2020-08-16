package com.rtjvm.scala.oop.files

class File(override val parentPath: String, override val name: String, contents: String)
  extends DirEntry(parentPath, name) {

  override def asDirectory: Directory =
    throw new FilesystemException("File cannot be converted to a directory")

  override def getType: String = "File"

  override def asFile: File =
    this
}

object File {

  def empty(parentPath: String, name: String): File =
    new File(parentPath, name, "")
}
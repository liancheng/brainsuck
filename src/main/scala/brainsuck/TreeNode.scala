package brainsuck

trait TreeNode[BaseType <: TreeNode[BaseType]] { this: BaseType =>
  def children: Seq[BaseType]

  private def withChildren(otherChildren: Seq[BaseType]): BaseType = {
    def sameChildren(otherChildren: Seq[BaseType]): Boolean =
      children.size == otherChildren.size && children.lazyZip(otherChildren).forall(_ == _)
    if (sameChildren(otherChildren)) this else makeCopy(otherChildren)
  }

  protected def makeCopy(args: Seq[BaseType]): BaseType

  def transformDown(rule: PartialFunction[BaseType, BaseType]): BaseType = {
    val selfTransformed = rule.applyOrElse(this, identity[BaseType])
    if (this == selfTransformed) this transformChildrenDown rule
    else selfTransformed transformChildrenDown rule
  }

  def transformChildrenDown(rule: PartialFunction[BaseType, BaseType]): BaseType =
    this withChildren children.map(_ transformDown rule)

  def transformUp(rule: PartialFunction[BaseType, BaseType]): BaseType = {
    val childrenTransformed = transformChildrenUp(rule)
    if (this == childrenTransformed) rule.applyOrElse(this, identity[BaseType])
    else rule.applyOrElse(childrenTransformed, identity[BaseType])
  }

  private def transformChildrenUp(rule: PartialFunction[BaseType, BaseType]): BaseType =
    this withChildren children.map(_ transformUp rule)
}

trait LeafNode[BaseType <: TreeNode[BaseType]] extends TreeNode[BaseType] { this: BaseType =>
  override def children = Seq.empty[BaseType]
  override def makeCopy(args: Seq[BaseType]): BaseType = this
}

trait UnaryNode[BaseType <: TreeNode[BaseType]] extends TreeNode[BaseType] { this: BaseType =>
  def child: BaseType
  override def children = Seq(child)
}

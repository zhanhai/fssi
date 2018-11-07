package fssi.store.mpt

import fssi.store.core.KVStore

trait MPT {
  private[mpt] def name: String
  private[mpt] def store: KVStore

  // user's method, put
  def put(k: Array[Byte], v: Array[Byte]): Unit = {
    val hash = Hash.encode(k)
    val key  = Key.encode(hash)
    insertTrie(key, Data.wrap(v))
  }
  // user's method, get
  def get(k: Array[Byte]): Option[Array[Byte]] = {
    val hash = Hash.encode(k)
    val key  = Key.encode(hash)
    fetchTrie(key).map(_.bytes)
  }

  private def insertTrie(key: Key, data: Data): Unit = {
    store.transact { proxy =>
      val (n, k)          = rootNode.getOrElse((Node.Null, Key.empty))
      val leaf            = Node.leaf(k.toPath, data)
      val (_, newRootKey) = saveNode(combineNode(leaf, n, proxy), proxy)
      if (k.nonEmpty) {
        proxy.delete(k.bytes)
      }
      // set new root node
      store.put(name.getBytes("utf-8"), newRootKey.bytes) match {
        case Left(t) => throw t
        case _       => ()
      }

    } match {
      case Left(t) => throw t
      case _       => ()
    }

  }

  private def fetchTrie(key: Key): Option[Data] = {

    def _fetch(path: Path, node: Node): Option[Data] = node match {
      case Node.Null                            => None
      case Node.Leaf(p, data) if path === p     => Some(data)
      case Node.Leaf(_, _)                      => None
      case Node.Branch(_, data) if path.isEmpty => Some(data)
      case Node.Branch(slot, _) =>
        val cell = slot.get(path.head.toInt)
        if (cell.key.isEmpty) None
        else {
          val next = for {
            data <- store.get(cell.key.bytes).map(Data.wrap)
            n    <- data.toNode
          } yield n
          if (next.isEmpty) None
          else _fetch(path.dropHead, next.get)
        }
      case Node.Extension(p, k) if p.length == 0 && path.length == 0 =>
        val next = for {
          data <- store.get(k.bytes).map(Data.wrap)
          n    <- data.toNode
        } yield n
        if (next.isEmpty) None
        else _fetch(path, next.get)
      case Node.Extension(p, _) if !p.hasPrefix(path) => None
      case Node.Extension(p, k) =>
        val prefix = p.prefix(path)
        _fetch(path.drop(prefix), Node.extension(p.drop(prefix), k))
    }

    rootNode.flatMap(x => _fetch(Path.plain(key.bytes), x._1))
  }

  private def combineNode(node1: Node, node2: Node, proxy: KVStore#Proxy): Node =
    (node1, node2) match {
      case (Node.Null, Node.Null) => Node.Null

      case (x, Node.Null) => x
      case (Node.Null, x) => x

      case (x @ Node.Leaf(_, _), y @ Node.Leaf(_, _))           => combineNode(x, y, proxy)
      case (x @ Node.Branch(_, _), y @ Node.Branch(_, _))       => combineNode(x, y, proxy)
      case (x @ Node.Extension(_, _), y @ Node.Extension(_, _)) => combineNode(x, y, proxy)

      case (x @ Node.Leaf(_, _), y @ Node.Branch(_, _))    => combineNode(x, y, proxy)
      case (x @ Node.Leaf(_, _), y @ Node.Extension(_, _)) => combineNode(x, y, proxy)

      case (x @ Node.Branch(_, _), y @ Node.Leaf(_, _))      => combineNode(y, x, proxy)
      case (x @ Node.Branch(_, _), y @ Node.Extension(_, _)) => combineNode(x, y, proxy)

      case (x @ Node.Extension(_, _), y @ Node.Branch(_, _)) => combineNode(y, x, proxy)
      case (x @ Node.Extension(_, _), y @ Node.Leaf(_, _))   => combineNode(y, x, proxy)

    }

  private def rootNode: Option[(Node, Key)] = {
    for {
      rootKey      <- store.get(name.getBytes("utf-8"))
      rootNodeData <- store.get(rootKey).map(Data.wrap)
      rootNode     <- rootNodeData.toNode
    } yield (rootNode, Key.wrap(rootKey))
  }

  private def saveNode[N <: Node](node: N, proxy: KVStore#Proxy): (N, Key) = {
    val data = Data.encode(node)
    val key  = Key.encode(data.hash)
    proxy.put(key.bytes, data.bytes)
    (node, key)
  }

  private def combineNode(node1: Node.Leaf, node2: Node.Leaf, proxy: KVStore#Proxy): Node = {
    if (node1.path === node2.path) node1
    else {
      val prefix = node1.path.prefix(node2.path)
      val branchData =
        if (prefix === node1.path) node1.data
        else if (prefix === node2.path) node2.data
        else Data.empty

      val branch = {
        val br = Node.branch(Slot.Hex.empty, branchData)

        val br1 =
          if (prefix === node1.path) br
          else {
            val leaf = Node.leaf(node1.path.drop(prefix), node1.data)
            combineNode(leaf, br, proxy)
          }

        val br2 =
          if (prefix == node2.path) br1
          else {
            val leaf = Node.leaf(node2.path.drop(prefix), node2.data)
            combineNode(leaf, br1, proxy)
          }
        br2
      }
      val (_, branchKey) = saveNode(branch, proxy)
      Node.extension(prefix, branchKey)
    }
  }

  private def combineNode(node1: Node.Leaf, node2: Node.Branch, proxy: KVStore#Proxy): Node = {
    val node1head = node1.path.head
    val slotCell  = node2.slot.get(node1head.toInt)
    if (slotCell.key.isEmpty) {
      if (node1.path.length > 1) {
        // if the length > 1, we can build a leaf node
        val (_, leafKey) = saveNode(Node.leaf(node1.path.dropHead, node1.data), proxy)
        val newSlot      = node2.slot.update(node1head, leafKey)
        Node.branch(newSlot, node2.data)
      } else {
        // if the length == 1, we have to build a empty branch node with data = node1.data
        val (_, branchHash) = saveNode(Node.branch(Slot.Hex.empty, node1.data), proxy)
        val newSlot         = node2.slot.update(node1head, branchHash)
        Node.branch(newSlot, node2.data)
      }
    } else {
      if (node1.path.length > 1) {
        // if the length > 1 we can build a leaf node , then combine it to old node
        val leaf = Node.leaf(node1.path.dropHead, node1.data)
        val old = for {
          data <- proxy.get(slotCell.key.bytes).map(Data.wrap)
          node <- data.toNode
        } yield node
        val (_, newSlotKey) = saveNode(combineNode(leaf, old.get, proxy), proxy)
        proxy.delete(slotCell.key.bytes)
        val newSlot = node2.slot.update(node1head, newSlotKey)
        Node.branch(newSlot, node2.data)
      } else {
        // if the length == 1, we have to build a empty branch node with data = node1.data, and combine old to this one
        val old = for {
          data <- proxy.get(slotCell.key.bytes).map(Data.wrap)
          node <- data.toNode
        } yield node

        val branch            = Node.branch(Slot.Hex.empty, node1.data)
        val (_, newBranchKey) = saveNode(combineNode(old.get, branch, proxy), proxy)
        proxy.delete(slotCell.key.bytes)
        val newSlot = node2.slot.update(node1head, newBranchKey)
        Node.branch(newSlot, node2.data)
      }
    }
  }
  private def combineNode(node1: Node.Leaf, node2: Node.Extension, proxy: KVStore#Proxy): Node = {
    if (node1.path === node2.path) node1
    else {
      val prefix = node1.path.prefix(node2.path)
      if (prefix.length == 0) {
        // no common prefix, build a empty branch, combine them to it
        val branch = Node.branch(Slot.Hex.empty, Data.empty)
        val br1    = combineNode(node1, branch, proxy)
        combineNode(node2, br1, proxy)
      } else {
        val n1remainPath = node1.path.drop(prefix)
        val n2remainPath = node2.path.drop(prefix)
        if (n1remainPath.nonEmpty && n2remainPath.nonEmpty) {
          val br0        = Node.branch(Slot.Hex.empty, Data.empty)
          val leaf       = Node.leaf(n1remainPath, node1.data)
          val br1        = combineNode(leaf, br0, proxy)
          val ext        = Node.extension(n2remainPath, node2.key)
          val (_, brKey) = saveNode(combineNode(ext, br1, proxy), proxy)
          // return a extension node with key referring to brKey
          Node.extension(prefix, brKey)
        } else if (n1remainPath.isEmpty) {
          val br0        = Node.branch(Slot.Hex.empty, node1.data)
          val ext        = Node.extension(n2remainPath, node2.key)
          val (_, brKey) = saveNode(combineNode(ext, br0, proxy), proxy)
          // return a extension node with key referring to brKey
          Node.extension(prefix, brKey)
        } else {
          //n2remainPath is empty
          val leaf = Node.leaf(n1remainPath, node1.data)
          val old = for {
            data <- proxy.get(node2.key.bytes).map(Data.wrap)
            node <- data.toNode
          } yield node
          proxy.delete(node2.key.bytes)
          val (_, newKey) = saveNode(combineNode(leaf, old.get, proxy), proxy)
          node2.copy(key = newKey)
        }
      }
    }
  }

  private def combineNode(node1: Node.Branch, node2: Node.Extension, proxy: KVStore#Proxy): Node = {
    throw new RuntimeException("branch node can't be combined with extension node")
  }

  private def combineNode(node1: Node.Extension,
                          node2: Node.Extension,
                          proxy: KVStore#Proxy): Node = {
    if (node1.path === node2.path) node1
    else {
      val prefix = node1.path.prefix(node2.path)
      if (prefix.length == 0) {
        val br0 = Node.branch(Slot.Hex.empty, Data.empty)
        val br1 = combineNode(node1, br0, proxy)
        combineNode(node2, br1, proxy)
      } else {
        val n1remainPath = node1.path.drop(prefix)
        val n2remainPath = node2.path.drop(prefix)
        if (n1remainPath.isEmpty) {
          val ext = Node.extension(n2remainPath, node2.key)
          val old = for {
            data <- proxy.get(node1.key.bytes).map(Data.wrap)
            node <- data.toNode
          } yield node
          proxy.delete(node1.key.bytes)
          combineNode(ext, old.get, proxy)
        } else if (n2remainPath.isEmpty) {
          val ext = Node.extension(n1remainPath, node1.key)
          val old = for {
            data <- proxy.get(node2.key.bytes).map(Data.wrap)
            node <- data.toNode
          } yield node
          proxy.delete(node2.key.bytes)
          combineNode(ext, old.get, proxy)
        } else {
          val br0  = Node.branch(Slot.Hex.empty, Data.empty)
          val ext1 = Node.extension(n1remainPath, node1.key)
          val br1  = combineNode(ext1, br0, proxy)
          val ext2 = Node.extension(n2remainPath, node2.key)
          combineNode(ext2, br1, proxy)
        }
      }
    }
  }
  private def combineNode(node1: Node.Branch, node2: Node.Branch, proxy: KVStore#Proxy): Node = {
    throw new RuntimeException("branch node can't be combined with another branch node")
  }

}
/*!
This crate provides object-oriented inheritance using Nodes.

A Node consits of a set of components and a Node can have other
children nodes.

# Defining classes

Use the [`class!`] invocation for defining class.

# Components

```
use oop_inheritance::*;

let something = Node::new();

// Set a component
something.set::<f64>(10);

// `Option<Arc<f64>>`
something.get::<f64>();
```

# Children

```ignore
something.add_child(&another_thing);
```

# Node paths

Since Nodes are in hierarchy, every Node that has a name may be found when
using the `.resolve_path` method. However, there are special segments of an Node path that do not resolve
by name, which may be useful in some contexts:

- `.first` resolves to the first child of an Node;
- `.last` resolves to the last child of an Node;
- `..` resolves to the parent.

```
let last_sibling: Option<Node> = node.resolve_path("../.last");
```
*/

use std::{
    any::Any,
    sync::{Arc, RwLock, Weak},
    hash::Hash, fmt::{Debug, Display}, error::Error,
};

use by_address::ByAddress;

type Component = Arc<dyn Any + Send + Sync>;

#[doc(hidden)]
pub use oop_inheritance_proc::class_extends;

pub use oop_inheritance_proc::class;

pub mod util;

use self::util::VectorExtensions;

fn default<T: Default>() -> T {
    T::default()
}

/// Represents an node as a type managed by reference-counting.
pub struct Node {
    inner: Arc<NodeInner>,
}

impl Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Node")
    }
}

impl Hash for Node {
    /// Hashes the node by reference.
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ByAddress(Arc::clone(&self.inner)).hash(state)
    }
}

impl PartialEq for Node {
    /// Compares nodes by reference.
    /// > **Note**: This method does not compare the nodes by content.
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}

impl Eq for Node {}

impl Clone for Node {
    /// Clones the node by reference.
    /// > **Note**: This method does not clone the node by content.
    fn clone(&self) -> Self {
        Self { inner: Arc::clone(&self.inner) }
    }
}

impl AsRef<Node> for Node {
    fn as_ref(&self) -> &Node {
        self
    }
}

impl Node {
    pub fn new() -> Node {
        Self {
            inner: Arc::new(NodeInner {
                name: RwLock::new(None),
                parent: RwLock::new(default()),
                components: RwLock::new(vec![]),
                children: RwLock::new(vec![]),
            })
        }
    }

    /// Downgrades the node reference into a weak reference.
    pub fn downgrade_ref(&self) -> WeakNodeRef {
        WeakNodeRef(Arc::downgrade(&self.inner))
    }

    /// Checks whether node has a specified component.
    pub fn has<T>(&self) -> bool
        where T: Any + Send + Sync
    {
        self.get::<T>().is_some()
    }

    /// Retrieves a component from the node.
    pub fn get<T>(&self) -> Option<Arc<T>>
        where T: Any + Send + Sync
    {
        for component in self.inner.components.read().unwrap().iter() {
            if let Ok(c) = Arc::downcast::<T>(Arc::clone(component)) {
                return Some(c);
            }
        }
        None
    }

    /// Overrides a component of the node. This method is chainable.
    pub fn set<T>(&self, value: T) -> Self
        where T: Any + Send + Sync
    {
        self.delete::<T>();
        self.inner.components.write().unwrap().push(Arc::new(value));
        self.clone()
    }

    /// Deletes a component of the node.
    /// Returns `true` if any component was deleted;
    /// otherwise returns `false`.
    pub fn delete<T>(&self) -> bool
        where T: Any + Send + Sync
    {
        let mut i = 0;
        let mut components = vec![];
        for component in self.inner.components.read().unwrap().iter() {
            components.push(Arc::clone(component));
        }
        for component in components {
            if Arc::downcast::<T>(Arc::clone(&component)).is_ok() {
                self.inner.components.write().unwrap().remove(i);
                return true;
            }
            i += 1;
        }
        false
    }
    
    pub fn parent(&self) -> Option<Node> {
        self.inner.parent.read().unwrap().upgrade()
    }

    pub fn children(&self) -> Vec<Node> {
        let mut c = vec![];
        for child in self.inner.children.read().unwrap().iter() {
            c.push(child.clone());
        }
        c
    }

    pub fn child_at(&self, index: usize) -> Option<Node> {
        if index < self.num_children() { Some(self.inner.children.read().unwrap()[index].clone()) } else { None }
    }

    /// Returns the number of children.
    pub fn num_children(&self) -> usize {
        self.inner.children.read().unwrap().len()
    }

    fn is_child_of(&self, child: &Node) -> bool {
        if let Some(p) = self.parent() {
            if &p == child {
                return true;
            }
        }
        for i in 0..child.num_children() {
            let child = child.child_at(i).unwrap();
            if self.is_child_of(&child) {
                return true;
            }
        }
        false
    }

    /// Adds a child node to the end of the children collection.
    /// If `child` is already child of an node, it is removed and then added
    /// as part of this node.
    pub fn add_child(&self, child: impl AsRef<Node>) {
        let child = child.as_ref();
        child.remove_from_parent();

        // Do not allow circular children
        assert!(!self.is_child_of(child), "Adding circular child.");

        *child.inner.parent.write().unwrap() = self.downgrade_ref();
        self.inner.children.write().unwrap().push(child.clone());
    }

    /// Adds a child node at the index `index` of the children collection.
    /// If `child` is already child of an node, it is removed and then added
    /// as part of this node.
    /// 
    /// # Panics
    /// 
    /// This method panics if `index` is out of bounds.
    pub fn add_child_at(&self, index: usize, child: impl AsRef<Node>) {
        let child = child.as_ref();
        child.remove_from_parent();
        assert!(index < self.num_children(), "Specified index is out of bounds.");

        // Do not allow circular children
        assert!(!self.is_child_of(child), "Adding circular child.");

        *child.inner.parent.write().unwrap() = self.downgrade_ref();
        self.inner.children.write().unwrap().insert(index, child.clone());
    }

    /// Adds a sequence of children to the end of the children collection.
    /// This is equivalent to iterating the sequence and invoking `add_child()`
    /// with every child.
    pub fn add_children(&self, children: impl IntoIterator<Item = impl AsRef<Node>>) {
        for child in children.into_iter() {
            self.add_child(child.as_ref());
        }
    }

    /// Swaps two children.
    /// 
    /// # Panics
    /// 
    /// Panics if any of the specified nodes is not part of the node.
    pub fn swap_children(&self, child_1: impl AsRef<Node>, child_2: impl AsRef<Node>) {
        let child_1 = child_1.as_ref();
        let child_2 = child_2.as_ref();
        let indices = [self.inner.children.read().unwrap().index_of(child_1), self.inner.children.read().unwrap().index_of(child_2)];
        assert!(indices.iter().all(|i| i.is_some()), "Some of the specified indices are out of bounds.");
        self.inner.children.write().unwrap().swap(indices[0].unwrap(), indices[1].unwrap());
    }

    /// Swaps two children.
    /// 
    /// # Panics
    /// 
    /// Panics if any of the specified indices is out of bounds.
    pub fn swap_children_by_indices(&self, child_1: usize, child_2: usize) {
        assert!([child_1, child_2].iter().all(|&i| i < self.num_children()), "Some of the specified indices are out of bounds.");
        self.inner.children.write().unwrap().swap(child_1, child_2);
    }

    /// Removes a child. Returns `true` if the child has been removed, or `false` otherwise.
    pub fn remove_child(&self, child: impl AsRef<Node>) -> bool {
        let child = child.as_ref();
        let i = self.inner.children.read().unwrap().index_of(child);
        if let Some(i) = i {
            self.inner.children.write().unwrap().remove(i);
            *child.inner.parent.write().unwrap() = default();
            true
        } else {
            false
        }
    }

    /// Removes all children nodes from the node.
    pub fn remove_children(&self) {
        for child in self.children() {
            *child.inner.parent.write().unwrap() = default();
        }
        self.inner.children.write().unwrap().clear();
    }

    /// Removes the node from its parent. Returns `true` if the child has been removed, or `false` otherwise.
    pub fn remove_from_parent(&self) -> bool {
        if let Some(p) = self.parent() { p.remove_child(self) } else { false }
    }

    /// The name of the node as used in Node paths.
    pub fn name(&self) -> Option<String> {
        self.inner.name.read().unwrap().clone()
    }

    /// The name of the node as used in Node paths.
    pub fn set_name(&self, name: Option<String>) {
        *self.inner.name.write().unwrap() = name;
    }

    /**
    Resolves an Node path. An Node path is resolved as follows:

    1. Let *segments* be the splitting of the path by the slash character (`/`).
    2. Let *r* be the initial node.
    3. For every segment *s*:
        1. If `s == ".first"`, let *r* be the first child of *r* or otherwise `None`.
        2. If `s == ".last"`, let *r* be the last child of *r* or otherwise `None`.
        3. If `s == ".."`, let *r* be the parent of *r* or otherwise `None`.
        4. If *s* is non-empty, let *r* be a child of *r* such that `child.name() == s` or otherwise `None`.
    4. Return *r*
    */
    pub fn resolve_path(&self, path: &str) -> Option<Node> {
        let segments = path.split('/');
        let mut r: Option<Node> = Some(self.clone());
        for s in segments {
            if r.is_none() {
                break;
            }
            match s {
                ".first" => {
                    r = r.unwrap().children().first().map(|c| c.clone());
                },
                ".last" => {
                    r = r.unwrap().children().last().map(|c| c.clone());
                },
                ".." => {
                    r = r.unwrap().parent();
                },
                "" => {
                    // Empty
                },
                _ => {
                    r = r.unwrap().children().iter().find(|c| c.name().as_ref().map(|cn| cn.as_ref()) == Some(s)).map(|c| c.clone());
                },
            }
        }
        r
    }

    /// Indicates whether an Node is of a certain Node subtype.
    pub fn is<T: TryFrom<Self, Error = ClassError>>(&self) -> bool {
        T::try_from(self.clone()).is_ok()
    }

    /// Attempts to convert this Node reference into a `T` reference.
    pub fn to<T: TryFrom<Self, Error = ClassError>>(&self) -> Result<T, ClassError> {
        T::try_from(self.clone())
    }
}

struct NodeInner {
    name: RwLock<Option<String>>,
    parent: RwLock<WeakNodeRef>,
    components: RwLock<Vec<Component>>,
    children: RwLock<Vec<Node>>,
}

/// Represents a weak reference to an node.
pub struct WeakNodeRef(Weak<NodeInner>);

impl WeakNodeRef {
    /// Returns a `WeakNodeRef` reference that upgrades to no
    /// strong reference.
    pub fn empty() -> Self {
        Self(Weak::new())
    }

    /// Attempts to upgrade a weak reference into a strong reference.
    pub fn upgrade(&self) -> Option<Node> {
        if let Some(r) = self.0.upgrade() { Some(Node { inner: r }) } else { None }
    }
}

impl Debug for WeakNodeRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "WeakNodeRef")
    }
}

impl Default for WeakNodeRef {
    fn default() -> Self {
        Self::empty()
    }
}

impl PartialEq for WeakNodeRef {
    /// Compares nodes by reference.
    fn eq(&self, other: &Self) -> bool {
        Weak::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for WeakNodeRef {}

impl Clone for WeakNodeRef {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

/// Represents an error originated from Node subclass relationships.
/// For example, this error might occur as result of a failed conversion.
pub struct ClassError {
    message: String,
}

impl ClassError {
    pub fn new(message: &str) -> Self {
        Self { message: message.into() }
    }
}

impl Display for ClassError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Debug for ClassError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as Display>::fmt(self, f)
    }
}

impl Error for ClassError {}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_components() {
        let node = Node::new();
        node.set(10.0);
        assert_eq!(10.0, *node.get::<f64>().unwrap());

        node.delete::<f64>();
        assert!(node.get::<f64>().is_none());
    }

    #[test]
    fn test_hierarchy() {
        let topmost = Node::new();
        let child_1 = Node::new();
        child_1.set_name(Some("child1".into()));
        topmost.add_child(&child_1);
        assert_eq!("child1".to_owned(), topmost.resolve_path(".last").unwrap().name().unwrap());
        assert_eq!(topmost.resolve_path(".last").unwrap(), child_1);
    }

    #[test]
    fn test_class_extends() {
        struct A(Node);

        class_extends!(A < Node, use AComponent, crate);
        
        impl A {
            fn new() -> Self {
                Self(Node::new().set(AComponent))
            }
        }

        struct AComponent;

        struct B(A);

        class_extends!(B < A < Node, use BComponent, crate);

        impl B {
            fn new() -> Self {
                Self(A::new().set(BComponent).try_into().unwrap())
            }
        }

        struct BComponent;

        let r = B::new();
        let r_e: Node = r.clone().into();
        let _: A = r.into();
        assert!(r_e.is::<B>());

        let r = Node::new();
        assert!(!r.is::<A>());
    }

    #[test]
    fn test_class() {
        class! {
            use oop_inheritance = crate;
            struct A: Node {
                x: f64 = 0.0,
            }
            fn constructor(x: f64) {
                super();
                this.set_x(x);
            }
        }

        let o = A::new(10.0);
        assert_eq!(o.x(), 10.0);

        class! {
            use oop_inheritance = crate;
            struct B: A < Node {
                y: A = A::new(15.0),
                ref z: f64 = 0.0,
            }
            fn constructor() {
                super(0.0);
            }
        }

        let o = B::new();
        assert_eq!(o.y().x(), 15.0);
    }
}
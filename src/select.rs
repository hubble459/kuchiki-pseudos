use crate::attributes::ExpandedName;
use crate::iter::{NodeIterator, Select};
use crate::node_data_ref::NodeDataRef;
use crate::tree::{ElementData, Node, NodeData, NodeRef};
use cssparser::{self, CowRcStr, ParseError, SourceLocation, ToCss};
use html5ever::{LocalName, Namespace};
use selectors::attr::{AttrSelectorOperation, CaseSensitivity, NamespaceConstraint};
use selectors::context::QuirksMode;
use selectors::parser::SelectorParseErrorKind;
use selectors::parser::{
    NonTSPseudoClass, Parser, Selector as GenericSelector, SelectorImpl, SelectorList,
};
use selectors::{self, matching, OpaqueElement};
use std::fmt;

/// The definition of whitespace per CSS Selectors Level 3 § 4.
///
/// Copied from rust-selectors.
static SELECTOR_WHITESPACE: &[char] = &[' ', '\t', '\n', '\r', '\x0C'];

#[derive(Debug, Clone)]
pub struct KuchikiSelectors;

impl SelectorImpl for KuchikiSelectors {
    type AttrValue = String;
    type Identifier = LocalName;
    type ClassName = LocalName;
    type LocalName = LocalName;
    type PartName = LocalName;
    type NamespacePrefix = LocalName;
    type NamespaceUrl = Namespace;
    type BorrowedNamespaceUrl = Namespace;
    type BorrowedLocalName = LocalName;

    type NonTSPseudoClass = PseudoClass;
    type PseudoElement = PseudoElement;

    type ExtraMatchingData = ();
}

struct KuchikiParser;

impl<'i> Parser<'i> for KuchikiParser {
    type Impl = KuchikiSelectors;
    type Error = SelectorParseErrorKind<'i>;

    fn parse_non_ts_functional_pseudo_class<'t>(
        &self,
        name: CowRcStr<'i>,
        arguments: &mut cssparser::Parser<'i, 't>,
    ) -> Result<<Self::Impl as SelectorImpl>::NonTSPseudoClass, ParseError<'i, Self::Error>> {
        use self::PseudoClass::*;

        let text = arguments.expect_ident_or_string()?.to_string();

        match name.to_ascii_lowercase().to_string().as_str() {
            "has" => Ok(Has(text)),
            "has-not" => Ok(HasNot(text)),
            "contains" => Ok(Contains(text)),
            "icontains" => Ok(CaseInsensitiveContains(text)),
            _ => Err(arguments.new_custom_error(
                SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name),
            )),
        }
    }

    fn parse_non_ts_pseudo_class(
        &self,
        location: SourceLocation,
        name: CowRcStr<'i>,
    ) -> Result<PseudoClass, ParseError<'i, SelectorParseErrorKind<'i>>> {
        use self::PseudoClass::*;

        match name.to_ascii_lowercase().to_string().as_str() {
            "any-link" => Ok(AnyLink),
            "link" => Ok(Link),
            "visited" => Ok(Visited),
            "active" => Ok(Active),
            "focus" => Ok(Focus),
            "hover" => Ok(Hover),
            "enabled" => Ok(Enabled),
            "disabled" => Ok(Disabled),
            "checked" => Ok(Checked),
            "indeterminate" => Ok(Indeterminate),

            "empty" => Ok(Empty),
            "not-empty" => Ok(NotEmpty),
            _ => Err(location.new_custom_error(
                SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name),
            )),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub enum PseudoClass {
    AnyLink,
    Link,
    Visited,
    Active,
    Focus,
    Hover,
    Enabled,
    Disabled,
    Checked,
    Indeterminate,

    // Custom
    Empty,
    NotEmpty,
    /// Implementation of [:not(selector)] selector
    HasNot(String),
    /// Implementation of [:has(selector)] selector
    Has(String),
    /// Implementation of [:contains("value")] selector
    Contains(String),
    /// Implementation of [:icontains("value")] selector
    CaseInsensitiveContains(String),
}

impl NonTSPseudoClass for PseudoClass {
    type Impl = KuchikiSelectors;

    fn is_active_or_hover(&self) -> bool {
        matches!(*self, PseudoClass::Active | PseudoClass::Hover)
    }

    fn is_user_action_state(&self) -> bool {
        matches!(
            *self,
            PseudoClass::Active | PseudoClass::Hover | PseudoClass::Focus
        )
    }

    fn has_zero_specificity(&self) -> bool {
        false
    }
}

impl ToCss for PseudoClass {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        dest.write_str(
            match self.clone() {
                PseudoClass::AnyLink => ":any-link".to_owned(),
                PseudoClass::Link => ":link".to_owned(),
                PseudoClass::Visited => ":visited".to_owned(),
                PseudoClass::Active => ":active".to_owned(),
                PseudoClass::Focus => ":focus".to_owned(),
                PseudoClass::Hover => ":hover".to_owned(),
                PseudoClass::Enabled => ":enabled".to_owned(),
                PseudoClass::Disabled => ":disabled".to_owned(),
                PseudoClass::Checked => ":checked".to_owned(),
                PseudoClass::Indeterminate => ":indeterminate".to_owned(),

                // Custom
                PseudoClass::Empty => ":empty".to_owned(),
                PseudoClass::NotEmpty => ":not-empty".to_owned(),
                PseudoClass::Contains(selector) => format!(":contains({selector})"),
                PseudoClass::CaseInsensitiveContains(selector) => format!(":icontains({selector})"),
                PseudoClass::Has(selector) => format!(":has({selector})"),
                PseudoClass::HasNot(selector) => format!(":has-not({selector})"),
            }
            .to_string()
            .as_str(),
        )
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub enum PseudoElement {}

impl ToCss for PseudoElement {
    fn to_css<W>(&self, _dest: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        match *self {}
    }
}

impl selectors::parser::PseudoElement for PseudoElement {
    type Impl = KuchikiSelectors;
}

impl selectors::Element for NodeDataRef<ElementData> {
    type Impl = KuchikiSelectors;

    #[inline]
    fn opaque(&self) -> OpaqueElement {
        let node: &Node = self.as_node();
        OpaqueElement::new(node)
    }

    #[inline]
    fn is_html_slot_element(&self) -> bool {
        false
    }
    #[inline]
    fn parent_node_is_shadow_root(&self) -> bool {
        false
    }
    #[inline]
    fn containing_shadow_host(&self) -> Option<Self> {
        None
    }

    #[inline]
    fn parent_element(&self) -> Option<Self> {
        self.as_node().parent().and_then(NodeRef::into_element_ref)
    }
    #[inline]
    fn prev_sibling_element(&self) -> Option<Self> {
        self.as_node().preceding_siblings().elements().next()
    }
    #[inline]
    fn next_sibling_element(&self) -> Option<Self> {
        self.as_node().following_siblings().elements().next()
    }
    #[inline]
    fn is_empty(&self) -> bool {
        self.as_node().children().all(|child| match *child.data() {
            NodeData::Element(_) => false,
            NodeData::Text(ref text) => text.borrow().is_empty(),
            _ => true,
        })
    }
    #[inline]
    fn is_root(&self) -> bool {
        match self.as_node().parent() {
            None => false,
            Some(parent) => matches!(*parent.data(), NodeData::Document(_)),
        }
    }

    #[inline]
    fn is_html_element_in_html_document(&self) -> bool {
        // FIXME: Have a notion of HTML document v.s. XML document?
        self.name.ns == ns!(html)
    }

    #[inline]
    fn has_local_name(&self, name: &LocalName) -> bool {
        self.name.local == *name
    }
    #[inline]
    fn has_namespace(&self, namespace: &Namespace) -> bool {
        self.name.ns == *namespace
    }

    #[inline]
    fn is_part(&self, _name: &LocalName) -> bool {
        false
    }

    #[inline]
    fn exported_part(&self, _: &LocalName) -> Option<LocalName> {
        None
    }

    #[inline]
    fn imported_part(&self, _: &LocalName) -> Option<LocalName> {
        None
    }

    #[inline]
    fn is_pseudo_element(&self) -> bool {
        false
    }

    #[inline]
    fn is_same_type(&self, other: &Self) -> bool {
        self.name == other.name
    }

    #[inline]
    fn is_link(&self) -> bool {
        self.name.ns == ns!(html)
            && matches!(
                self.name.local,
                local_name!("a") | local_name!("area") | local_name!("link")
            )
            && self
                .attributes
                .borrow()
                .map
                .contains_key(&ExpandedName::new(ns!(), local_name!("href")))
    }

    #[inline]
    fn has_id(&self, id: &LocalName, case_sensitivity: CaseSensitivity) -> bool {
        self.attributes
            .borrow()
            .get(local_name!("id"))
            .map_or(false, |id_attr| {
                case_sensitivity.eq(id.as_bytes(), id_attr.as_bytes())
            })
    }

    #[inline]
    fn has_class(&self, name: &LocalName, case_sensitivity: CaseSensitivity) -> bool {
        let name = name.as_bytes();
        !name.is_empty()
            && if let Some(class_attr) = self.attributes.borrow().get(local_name!("class")) {
                class_attr
                    .split(SELECTOR_WHITESPACE)
                    .any(|class| case_sensitivity.eq(class.as_bytes(), name))
            } else {
                false
            }
    }

    #[inline]
    fn attr_matches(
        &self,
        ns: &NamespaceConstraint<&Namespace>,
        local_name: &LocalName,
        operation: &AttrSelectorOperation<&String>,
    ) -> bool {
        let attrs = self.attributes.borrow();
        match *ns {
            NamespaceConstraint::Any => attrs
                .map
                .iter()
                .any(|(name, attr)| name.local == *local_name && operation.eval_str(&attr.value)),
            NamespaceConstraint::Specific(ns_url) => attrs
                .map
                .get(&ExpandedName::new(ns_url, local_name.clone()))
                .map_or(false, |attr| operation.eval_str(&attr.value)),
        }
    }

    fn match_pseudo_element(
        &self,
        pseudo: &PseudoElement,
        _context: &mut matching::MatchingContext<KuchikiSelectors>,
    ) -> bool {
        match *pseudo {}
    }

    fn match_non_ts_pseudo_class<F>(
        &self,
        pseudo: &PseudoClass,
        _context: &mut matching::MatchingContext<KuchikiSelectors>,
        _flags_setter: &mut F,
    ) -> bool
    where
        F: FnMut(&Self, matching::ElementSelectorFlags),
    {
        use self::PseudoClass::*;

        match pseudo.clone() {
            Active | Focus | Hover | Enabled | Disabled | Checked | Indeterminate | Visited => {
                false
            }
            AnyLink | Link => {
                self.name.ns == ns!(html)
                    && matches!(
                        self.name.local,
                        local_name!("a") | local_name!("area") | local_name!("link")
                    )
                    && self.attributes.borrow().contains(local_name!("href"))
            }
            Empty => self.is_empty(),
            NotEmpty => !self.is_empty(),
            HasNot(selector) => self.as_node().select_first(&selector).is_err(),
            Has(selector) => self.as_node().select_first(&selector).is_ok(),
            Contains(text) => self.text_contents().contains(&text),
            CaseInsensitiveContains(text) => self
                .text_contents()
                .to_ascii_lowercase()
                .contains(&text.to_ascii_lowercase()),
        }
    }
}

/// A pre-compiled list of CSS Selectors.
#[derive(Clone)]
pub struct Selectors(pub Vec<Selector>);

/// A pre-compiled CSS Selector.
#[derive(Clone)]
pub struct Selector(GenericSelector<KuchikiSelectors>);

/// The specificity of a selector.
///
/// Opaque, but ordered.
///
/// Determines precedence in the cascading algorithm.
/// When equal, a rule later in source order takes precedence.
#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Specificity(u32);

impl Selectors {
    /// Compile a list of selectors. This may fail on syntax errors or unsupported selectors.
    #[inline]
    pub fn compile(s: &str) -> Result<Selectors, ()> {
        let mut input = cssparser::ParserInput::new(s);
        match SelectorList::parse(&KuchikiParser, &mut cssparser::Parser::new(&mut input)) {
            Ok(list) => Ok(Selectors(list.0.into_iter().map(Selector).collect())),
            Err(_) => Err(()),
        }
    }

    /// Returns whether the given element matches this list of selectors.
    #[inline]
    pub fn matches(&self, element: &NodeDataRef<ElementData>) -> bool {
        self.0.iter().any(|s| s.matches(element))
    }

    /// Filter an element iterator, yielding those matching this list of selectors.
    #[inline]
    pub fn filter<I>(&self, iter: I) -> Select<I, &Selectors>
    where
        I: Iterator<Item = NodeDataRef<ElementData>>,
    {
        Select {
            iter,
            selectors: self,
        }
    }
}

impl Selector {
    /// Returns whether the given element matches this selector.
    #[inline]
    pub fn matches(&self, element: &NodeDataRef<ElementData>) -> bool {
        let mut context = matching::MatchingContext::new(
            matching::MatchingMode::Normal,
            None,
            None,
            QuirksMode::NoQuirks,
        );
        matching::matches_selector(&self.0, 0, None, element, &mut context, &mut |_, _| {})
    }

    /// Return the specificity of this selector.
    pub fn specificity(&self) -> Specificity {
        Specificity(self.0.specificity())
    }
}

impl ::std::str::FromStr for Selectors {
    type Err = ();
    #[inline]
    fn from_str(s: &str) -> Result<Selectors, ()> {
        Selectors::compile(s)
    }
}

impl fmt::Display for Selector {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.to_css(f)
    }
}

impl fmt::Display for Selectors {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut iter = self.0.iter();
        let first = iter
            .next()
            .expect("Empty Selectors, should contain at least one selector");
        first.0.to_css(f)?;
        for selector in iter {
            f.write_str(", ")?;
            selector.0.to_css(f)?;
        }
        Ok(())
    }
}

impl fmt::Debug for Selector {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Debug for Selectors {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

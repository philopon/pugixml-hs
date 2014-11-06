#include <pugixml.hpp>
#include <stdlib.h>
#include <string>
#include <cstring>
#include <sstream>

extern "C" {
  typedef pugi::xml_document       Document;
  typedef pugi::xml_node           Node;
  typedef pugi::xml_attribute      Attr;
  typedef pugi::xml_parse_result   ParseResult;
  typedef pugi::xpath_query        XPath;
  typedef pugi::xpath_node         XPathNode;
  typedef pugi::xpath_node_set     NodeSet;

  inline Node* checkNewNode(const Node& n) { if (n) {return new Node(n);} else {return NULL;} }
  inline Attr* checkNewAttr(const Attr& a) { if (a) {return new Attr(a);} else {return NULL;} }
  inline XPathNode* checkNewXPathNode(const XPathNode& n) { if (n) {return new XPathNode(n);} else {return NULL;} }

  int pugixml_version () { return PUGIXML_VERSION; }

  ////// enum xml_node_type
  int enum_node_type_null       () {return pugi::node_null;}
  int enum_node_type_document   () {return pugi::node_document;}
  int enum_node_type_element    () {return pugi::node_element;}
  int enum_node_type_pcdata     () {return pugi::node_pcdata;}
  int enum_node_type_cdata      () {return pugi::node_cdata;}
  int enum_node_type_comment    () {return pugi::node_comment;}
  int enum_node_type_pi         () {return pugi::node_pi;}
  int enum_node_type_declaration() {return pugi::node_declaration;}
  int enum_node_type_doctype    () {return pugi::node_doctype;}

  ////// enum xml_parse_status
  int enum_parse_status_ok                  () {return pugi::status_ok;}
  int enum_parse_status_file_not_found      () {return pugi::status_file_not_found;}
  int enum_parse_status_io_error            () {return pugi::status_io_error;}
  int enum_parse_status_out_of_memory       () {return pugi::status_out_of_memory;}
  int enum_parse_status_internal_error      () {return pugi::status_internal_error;}
  int enum_parse_status_unrecognized_tag    () {return pugi::status_unrecognized_tag;}
  int enum_parse_status_bad_pi              () {return pugi::status_bad_pi;}
  int enum_parse_status_bad_comment         () {return pugi::status_bad_comment;}
  int enum_parse_status_bad_cdata           () {return pugi::status_bad_cdata;}
  int enum_parse_status_bad_doctype         () {return pugi::status_bad_doctype;}
  int enum_parse_status_bad_pcdata          () {return pugi::status_bad_pcdata;}
  int enum_parse_status_bad_start_element   () {return pugi::status_bad_start_element;}
  int enum_parse_status_bad_attribute       () {return pugi::status_bad_attribute;}
  int enum_parse_status_bad_end_element     () {return pugi::status_bad_end_element;}
  int enum_parse_status_end_element_mismatch() {return pugi::status_end_element_mismatch;}
  int enum_parse_status_append_invalid_root () {return pugi::status_append_invalid_root;}
  int enum_parse_status_no_document_element () {return pugi::status_no_document_element;}

  ////// enum xml_encoding
  int enum_encoding_auto    () {return pugi::encoding_auto;}
  int enum_encoding_utf8    () {return pugi::encoding_utf8;}
  int enum_encoding_utf16_le() {return pugi::encoding_utf16_le;}
  int enum_encoding_utf16_be() {return pugi::encoding_utf16_be;}
  int enum_encoding_utf16   () {return pugi::encoding_utf16;}
  int enum_encoding_utf32_le() {return pugi::encoding_utf32_le;}
  int enum_encoding_utf32_be() {return pugi::encoding_utf32_be;}
  int enum_encoding_utf32   () {return pugi::encoding_utf32;}
  int enum_encoding_wchar   () {return pugi::encoding_wchar;}
  int enum_encoding_latin1  () {return pugi::encoding_latin1;}

  ////// enum xpath_value_type
  int enum_xpath_value_type_none    (){return pugi::xpath_type_none;}
  int enum_xpath_value_type_node_set(){return pugi::xpath_type_node_set;}
  int enum_xpath_value_type_number  (){return pugi::xpath_type_number;}
  int enum_xpath_value_type_string  (){return pugi::xpath_type_string;}
  int enum_xpath_value_type_boolean (){return pugi::xpath_type_boolean;}

  ////// formatting option bit flags
  unsigned int format_flag_default       (){return pugi::format_default;}
  unsigned int format_flag_indent        (){return pugi::format_indent;}
  unsigned int format_flag_no_declaration(){return pugi::format_no_declaration;}
  unsigned int format_flag_no_escapes    (){return pugi::format_no_escapes;}
  unsigned int format_flag_raw           (){return pugi::format_raw;}
  unsigned int format_flag_save_file_text(){return pugi::format_save_file_text;}
  unsigned int format_flag_write_bom     (){return pugi::format_write_bom;}

  ////// parsing option bit flags
  unsigned int parse_flag_cdata           (){return pugi::parse_cdata;}
  unsigned int parse_flag_comments        (){return pugi::parse_comments;}
  unsigned int parse_flag_declaration     (){return pugi::parse_declaration;}
  unsigned int parse_flag_default         (){return pugi::parse_default;}
  unsigned int parse_flag_doctype         (){return pugi::parse_doctype;}
  unsigned int parse_flag_eol             (){return pugi::parse_eol;}
  unsigned int parse_flag_escapes         (){return pugi::parse_escapes;}
  unsigned int parse_flag_fragment        (){return pugi::parse_fragment;}
  unsigned int parse_flag_full            (){return pugi::parse_full;}
  unsigned int parse_flag_minimal         (){return pugi::parse_minimal;}
  unsigned int parse_flag_pi              (){return pugi::parse_pi;}
  unsigned int parse_flag_trim_pcdata     (){return pugi::parse_trim_pcdata;}
  unsigned int parse_flag_ws_pcdata       (){return pugi::parse_ws_pcdata;}
  unsigned int parse_flag_ws_pcdata_single(){return pugi::parse_ws_pcdata_single;}
  unsigned int parse_flag_wconv_attribute (){return pugi::parse_wconv_attribute;}
  unsigned int parse_flag_wnorm_attribute (){return pugi::parse_wnorm_attribute;}

  ////// methods of document
  Document* new_document() { return new pugi::xml_document(); }
  void delete_document(const Document* doc) { delete doc; }

  void reset_document_with(Document* doc, const Document* proto){
    doc->reset(*proto);
  }

  ParseResult* load_buffer(Document* doc, const void* str, size_t size,
      unsigned int options, pugi::xml_encoding encoding) {
    return new pugi::xml_parse_result(doc->load_buffer(str, size, options, encoding));
  }

  ParseResult* load_file(Document* doc, const char* path,
      unsigned int options, pugi::xml_encoding encoding) {
    return new pugi::xml_parse_result(doc->load_file(path, options, encoding));
  }

  int save_file(const Document* doc, const char* path, const char* indent, 
      unsigned int flags, pugi::xml_encoding encoding) {
    return doc->save_file(path, indent, flags, encoding);
  }

  struct wrap_writer : pugi::xml_writer {
    void (*func)(const char* data, size_t size);

    virtual void write(const void* data, size_t size) {
      func(static_cast<const char*>(data), size);
    }
  };

  void save_string(const Document* doc, void (*func)(const char*, size_t),
      const char* indent, unsigned int flags, pugi::xml_encoding encoding) {
    wrap_writer wtr;
    wtr.func = func;
    doc->save(wtr, indent, flags, encoding);
  }

  Node* document_element(const Document* doc) { return checkNewNode(doc->document_element()); }

  ////// methods of xml_parse_result
  void delete_parse_result(const ParseResult* r) { delete r; }
  int  parse_is_success(const ParseResult* r) { return *r ? true : false; }
  int  parse_result_status(const ParseResult* r) { return r->status; }
  long parse_result_offset(const ParseResult* r) { return r->offset; }
  pugi::xml_encoding parse_result_encoding(const ParseResult* r) { return r->encoding; }
  const char* parse_result_description(const ParseResult* r) { return r->description(); }

  ////// methods of xml_attribute
  void delete_attr(const Attr* a) { delete a; }
  size_t attr_hash_value(const Attr* a) { return a->hash_value(); }

  const char* attr_name(const Attr* a) { return a->name(); }
  const char* attr_value(const Attr* a) { return a->value(); }

  int attr_set_value(Attr* a, const char* v) { return a->set_value(v); }

  ////// methods of node
  void delete_node(const Node* n) { delete n; }

  size_t node_hash_value(const void* n) { return static_cast<const Node*>(n)->hash_value(); }

  int node_type(const void* n) { return static_cast<const Node*>(n)->type(); }

  const char* node_name(const void* n)  { return static_cast<const Node*>(n)->name(); }
  const char* node_value(const void* n) { return static_cast<const Node*>(n)->value(); }

  Node* node_parent(const void* n) { return checkNewNode(static_cast<const Node*>(n)->parent()); }
  Node* node_first_child(const void* n) { return checkNewNode(static_cast<const Node*>(n)->first_child()); }
  Node* node_last_child(const void* n) { return checkNewNode(static_cast<const Node*>(n)->last_child()); }
  Node* node_next_sibling(const void* n) { return checkNewNode(static_cast<const Node*>(n)->next_sibling()); }
  Node* node_previous_sibling(const void* n) { return checkNewNode(static_cast<const Node*>(n)->previous_sibling()); }

  Node* node_child(const void* n, const char* name) { return checkNewNode(static_cast<const Node*>(n)->child(name)); }
  Attr* node_attribute(const void* n, const char* name) { return checkNewAttr(static_cast<const Node*>(n)->attribute(name)); }
  Node* node_next_sibling_by_name(const void* n, const char* name) { return checkNewNode(static_cast<const Node*>(n)->next_sibling(name)); }
  Node* node_previous_sibling_by_name(const void* n, const char* name) { return checkNewNode(static_cast<const Node*>(n)->previous_sibling(name)); }
  Node* node_find_child_by_name_and_attribute(const void* n, const char* name, const char* attr_name, const char* attr_value) { return checkNewNode(static_cast<const Node*>(n)->find_child_by_attribute(name, attr_name, attr_value)); }
  Node* node_find_child_by_attribute(const void* n, const char* attr_name, const char* attr_value) { return checkNewNode(static_cast<const Node*>(n)->find_child_by_attribute(attr_name, attr_value)); }

  const char* node_child_value(const void* n) { return static_cast<const Node*>(n)->child_value(); }
  const char* node_child_value_by_name(const void* n, const char* name) { return static_cast<const Node*>(n)->child_value(name); }
  const char* node_text(const void* n) { return static_cast<const Node*>(n)->text().get(); }

  typedef struct predicate {
    bool (*attr_pred)(const Attr*);
    bool (*node_pred)(const Node*);

    bool operator()(pugi::xml_attribute attr) const {
      return attr_pred(&attr);
    }

    bool operator()(pugi::xml_node node) const {
      return node_pred(&node);
    }
  } predicate_t;

  bool default_attr_pred (const Attr*) { return true; }
  bool default_node_pred (const Node*) { return true; }

  Attr* find_attribute(const void* node, bool(*attr_pred)(const Attr*)) {
    predicate_t pred;
    pred.attr_pred = attr_pred;
    pred.node_pred = &default_node_pred;
    return checkNewAttr(static_cast<const Node*>(node)->find_attribute(pred));
  }

  Node* find_child(const void* node, bool(node_pred)(const Node*)) {
    predicate_t pred;
    pred.attr_pred = default_attr_pred;
    pred.node_pred = node_pred;
    return checkNewNode(static_cast<const Node*>(node)->find_child(pred));
  }

  Node* find_node(const void* node, bool(node_pred)(const Node*)) {
    predicate_t pred;
    pred.attr_pred = default_attr_pred;
    pred.node_pred = node_pred;
    return checkNewNode(static_cast<const Node*>(node)->find_node(pred));
  }

  void node_map_sibling (const void* node, void (*fun)(Node*)) {
    const Node* n = static_cast<const Node*>(node);
    for(pugi::xml_node_iterator it = n->begin(); it != n->end(); ++it) {
      fun(&*it);
    }
  }
  void node_map_attributes (const void* node, void (*fun)(Attr*)) {
    const Node* n = static_cast<const Node*>(node);
    for(pugi::xml_attribute_iterator it = n->attributes_begin(); it != n->attributes_end(); ++it) {
      fun(&*it);
    }
  }

  char* node_path(const void* node, const char del) { 
    std::string s = static_cast<const Node*>(node)->path(del);
    char* ret = (char*)malloc(s.length() + 1);
    std::strcpy(ret, s.c_str());
    return ret;
  }

  Node* node_first_element_by_path(const void* node, const char* path, char del) {
    return checkNewNode(static_cast<const Node*>(node)->first_element_by_path(path, del));
  }

  Node* node_root(const void* n) {return checkNewNode(static_cast<const Node*>(n)->root()); }

  int set_name(void* n, const char* name) { return static_cast<Node*>(n)->set_name(name); }
  int set_value(void* n, const char* name) { return static_cast<Node*>(n)->set_value(name); }

  int append_attribute(void* n, const char* name, const char* val) { return static_cast<Node*>(n)->append_attribute(name).set_value(val); }
  int prepend_attribute(void* n, const char* name, const char* val) { return static_cast<Node*>(n)->append_attribute(name).set_value(val); }

  Node* append_child(void* n, pugi::xml_node_type typ) { return checkNewNode(static_cast<Node*>(n)->append_child(typ)); }
  Node* prepend_child(void* n, pugi::xml_node_type typ) { return checkNewNode(static_cast<Node*>(n)->prepend_child(typ)); }

  Node* append_copy(void* n, const Node* proto) { return checkNewNode(static_cast<Node*>(n)->append_copy(*proto)); }
  Node* prepend_copy(void* n, const Node* proto) { return checkNewNode(static_cast<Node*>(n)->prepend_copy(*proto)); }

  int remove_attribute(void* n, const char* name) { return static_cast<Node*>(n)->remove_attribute(name); }
  int remove_child   (void* n, const Node* cld) { return static_cast<Node*>(n)->remove_child(*cld); }

  ParseResult* append_buffer(void* node, const void* cont,
      size_t size, unsigned int options, pugi::xml_encoding enc) {
    return new pugi::xml_parse_result(static_cast<Node*>(node)->append_buffer(cont, size, options, enc));
  }

  void node_print(const void* node, void (*func)(const char*, size_t),
      const char* indent, unsigned int flags, pugi::xml_encoding encoding,
      unsigned int depth) {
    wrap_writer wtr;
    wtr.func = func;
    static_cast<const Node*>(node)->print(wtr, indent, flags, encoding, depth);
  }

  XPathNode* select_single_node(const void* node, const XPath* x) {
    return checkNewXPathNode(static_cast<const Node*>(node)->select_single_node(*x));
  }
  NodeSet* select_nodes(const void* node, const XPath* x) {
    return new NodeSet(static_cast<const Node*>(node)->select_nodes(*x));
  }

  ////// methods of xpath_node
  void delete_xpath_node(const XPathNode* p) { delete p; }
  Node* xpath_node_node(const XPathNode* p) { return checkNewNode(p->node()); }
  Attr* xpath_node_attribute(const XPathNode* p) { return checkNewAttr(p->attribute()); }

  ////// methods of xpath_node_set
  void delete_xpath_node_set(const NodeSet* p) { delete p; }
  size_t xpath_node_set_size  (const NodeSet* p) { return p->size(); }
  int    xpath_node_set_empty (const NodeSet* p) { return p->empty(); }
  XPathNode*  xpath_node_set_index (const NodeSet* p, size_t i) { 
    return checkNewXPathNode((*p)[i]);
  }
  void xpath_node_set_map(const NodeSet* p, void (*func)(const XPathNode*)) {
    for(pugi::xpath_node_set::const_iterator it = p->begin(); it != p->end(); ++it) {
      func(&*it);
    }
  }

  ////// methods of xpath_query
  void delete_xpath_query(const XPath* p) { delete p; }
  XPath* new_xpath_query_no_variable(const char* query) { return new pugi::xpath_query(query); }

  int xpath_query_evaluate_boolean(const XPath* p, const void* n) {
    XPathNode xn(*static_cast<const Node*>(n));
    return p->evaluate_boolean(xn); 
  }
  double xpath_query_evaluate_number(const XPath* p, const void* n) {
    XPathNode xn(*static_cast<const Node*>(n));
    return p->evaluate_number(xn);
  }

  char* xpath_query_evaluate_string(const XPath* p, const void* n) {
    XPathNode xn(*static_cast<const Node*>(n));
    std::string s = p->evaluate_string(xn);
    char* ret = (char*)malloc(s.length() + 1);
    std::strcpy(ret, s.c_str());
    return ret;
  }

  NodeSet* xpath_query_evaluate_node_set(const XPath* p, const void* n) {
    XPathNode xn(*static_cast<const Node*>(n));
    return new NodeSet(p->evaluate_node_set(xn));
  }

  pugi::xpath_value_type xpath_query_return_type(const XPath* p) { return p->return_type(); }
  int xpath_query_parse_is_success(const XPath* p) { return static_cast<bool>(*p); }

}

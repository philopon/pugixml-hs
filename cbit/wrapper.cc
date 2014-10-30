#include <pugixml.hpp>
#include <stdlib.h>
#include <string>
#include <sstream>

extern "C" {
  typedef pugi::xml_document     Document;
  typedef pugi::xml_node         Node;
  typedef pugi::xml_attribute    Attr;
  typedef pugi::xml_parse_result ParseResult;

  inline Node* checkNewNode(Node n) { if (n) {return new Node(n);} else {return NULL;} }
  inline Attr* checkNewAttr(Attr a) { if (a) {return new Attr(a);} else {return NULL;} }

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
  void delete_document(Document* doc) { delete doc; }

  void reset_document_with(Document* doc, const Document* proto){
    doc->reset(*proto);
  }

  ParseResult* load_string(Document* doc, const char* str, unsigned int options, pugi::xml_encoding encoding) {
    std::istringstream ist(str);
    return new pugi::xml_parse_result(doc->load(ist, options, encoding));
  }

  ParseResult* load_file(Document* doc, const char* path,
      unsigned int options, pugi::xml_encoding encoding) {
    return new pugi::xml_parse_result(doc->load_file(path, options, encoding));
  }

  int save_file(Document* doc, const char* path, const char* indent, 
      unsigned int flags, pugi::xml_encoding encoding) {
    return doc->save_file(path, indent, flags, encoding);
  }

  struct wrap_writer : pugi::xml_writer {
    void (*func)(const char* data, size_t size);

    virtual void write(const void* data, size_t size) {
      func(static_cast<const char*>(data), size);
    }
  };

  void save_string(Document* doc, void (*func)(const char*, size_t), const char* indent, 
      unsigned int flags, pugi::xml_encoding encoding) {
    wrap_writer wtr;
    wtr.func = func;
    doc->save(wtr, indent, flags, encoding);
  }

  ////// methods of xml_parse_result
  void delete_parse_result(ParseResult* r) { delete r; }
  int  parse_is_success(ParseResult* r) { return r ? true : false; }
  int  parse_result_status(ParseResult* r) { return r->status; }
  long parse_result_offset(ParseResult* r) { return r->offset; }
  pugi::xml_encoding parse_result_encoding(ParseResult* r) { return r->encoding; }
  const char* parse_result_description(ParseResult* r) { return r->description(); }

  ////// methods of xml_attribute
  void delete_attr(Attr* a) { delete a; }
  size_t attr_hash_value(Attr* a) { return a->hash_value(); }

  const char* attr_name(Attr* a) { return a->name(); }
  const char* attr_value(Attr* a) { return a->value(); }


  ////// methods of node
  void delete_node(Node* n) { delete n; }

  size_t node_hash_value(void* n) { return static_cast<Node*>(n)->hash_value(); }

  int node_type(void* n) { return static_cast<Node*>(n)->type(); }

  const char* node_name(void* n)  { return static_cast<Node*>(n)->name(); }
  const char* node_value(void* n) { return static_cast<Node*>(n)->value(); }

  Node* node_parent(void* n) { return checkNewNode(static_cast<Node*>(n)->parent()); }
  Node* node_first_child(void* n) { return checkNewNode(static_cast<Node*>(n)->first_child()); }
  Node* node_last_child(void* n) { return checkNewNode(static_cast<Node*>(n)->last_child()); }
  Node* node_next_sibling(void* n) { return checkNewNode(static_cast<Node*>(n)->next_sibling()); }
  Node* node_previous_sibling(void* n) { return checkNewNode(static_cast<Node*>(n)->previous_sibling()); }

  Node* node_child(void* n, const char* name) { return checkNewNode(static_cast<Node*>(n)->child(name)); }
  Attr* node_attribute(void* n, const char* name) { return checkNewAttr(static_cast<Node*>(n)->attribute(name)); }
  Node* node_next_sibling_by_name(void* n, const char* name) { return checkNewNode(static_cast<Node*>(n)->next_sibling(name)); }
  Node* node_previous_sibling_by_name(void* n, const char* name) { return checkNewNode(static_cast<Node*>(n)->previous_sibling(name)); }
  Node* node_find_child_by_name_and_attribute(void* n, const char* name, const char* attr_name, const char* attr_value) { return checkNewNode(static_cast<Node*>(n)->find_child_by_attribute(name, attr_name, attr_value)); }
  Node* node_find_child_by_attribute(void* n, const char* attr_name, const char* attr_value) { return checkNewNode(static_cast<Node*>(n)->find_child_by_attribute(attr_name, attr_value)); }

  const char* node_child_value(void* n) { return static_cast<Node*>(n)->child_value(); }
  const char* node_child_value_by_name(void* n, const char* name) { return static_cast<Node*>(n)->child_value(name); }
  const char* node_text(void* n) { return static_cast<Node*>(n)->text().get(); }

  void node_map_sibling (void* node, void (*fun)(Node*)) {
    Node* n = static_cast<Node*>(node);
    for(pugi::xml_node_iterator it = n->begin(); it != n->end(); ++it) {
      fun(&(*it));
    }
  }
  void node_map_attributes (void* node, void (*fun)(Attr*)) {
    Node* n = static_cast<Node*>(node);
    for(pugi::xml_attribute_iterator it = n->attributes_begin(); it != n->attributes_end(); ++it) {
      fun(&(*it));
    }
  }

  char* node_path(void* node, const char del) { 
    std::string s = static_cast<Node*>(node)->path(del);
    char* ret = (char*)malloc(s.length() + 1);
    std::strcpy(ret, s.c_str());
    return ret;
  }

  Node* node_first_element_by_path(void* node, const char* path, const char del) {
    return checkNewNode(static_cast<Node*>(node)->first_element_by_path(path, del));
  }

  Node* node_root(void* n) {return checkNewNode(static_cast<Node*>(n)->root()); }

}

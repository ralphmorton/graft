# graft

This is a general purpose minimalist templating library

```
Hello, this is a template.

You can bind a simple variable like this:

{{a_variable}}

You can use dot notation to access subfields of TemplateData Vars:

{{person.address.street}}

You can define a loop like this:

[[ person:people ]]
    Hello, {{person.name}}
[[ end ]]

Subtemplates can be referened by name:

<|sub_template.tpl|>

Or by variable lookup:

<|@named_subtemplate|>

Conditional rendering:

[[ ?render ]]
    This will be rendered if {{render}} exists
[[ end ]]

```

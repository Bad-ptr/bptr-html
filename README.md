# bptr-html

## Intro

Generate html from list structures.  
Depends on [`bptr-plist`](https://github.com/Bad-ptr/bptr-plist).

## Example
You can define tags with `bptr-html:deftag`. 'Defining a tag' means remembering it's parameters.  
But you can supply another value for parameter when constructing tag and that value will be used inplace.  
(here we define 'br tag to always use :xclose as t (empty keywords here means t) and in some place we use tag br with :xclose as nil).

```common-lisp
(bptr-html:deftag 'br :xclose)
(with-output-to-string (str)
           (bptr-html:WITH-HTML-OUTPUT str
             (bptr-html:html
              (name :asd "asd" :dsd "asdds" :editable :amputable :pre-tag (pretag "pretag")
                    :post-tag "posttag" :pre-body (prebd.cls#id "Hi!") :post-body post-body
                    :body (tagname.cls#aid :asd "adsd"
                                           :body (:mpp :class "cls"
                                                      "eshse body"
                                                      (br)
                                                      (br :xclose nil)
                                                      (:every :asd dfd "asdasd" (tag.clcl "body"))
                                                      (:escape-every "<html>test</html>")
                                                      (:escape (br))
                                                      ("tete" :body "ahaha")))))))
```

Will produce:

```html
<pretag>
  pretag
</pretag>

<name asd="asd" dsd="asdds" editable amputable>
  <prebd class="cls" id="id">
    Hi!
  </prebd>

  <tagname class="cls" id="aid" asd="adsd">
    <mpp class="cls">
      eshse body
      <br>
      </br>
      <br>
      </br>
      asddfdasdasd<tag class="clcl">
        body
      </tag>
      &lt;html&gt;test&lt;/html&gt;      &lt;br&gt;
      &lt;/br&gt;
      <tete>
        ahaha
      </tete>
    </mpp>
  </tagname>
  post-body
</name>
posttag
```
You can output without spaces/newlines. See `*html-human-readable*` and `*with-html-vars*`.  

Note bug with escaped text:

```
&lt;html&gt;test&lt;/html&gt;      &lt;br/&gt;
   here must be newline -->  ^
```

So if you wnat human-readably output for this you can try:

```common-lisp
(bptr-html:deftag 'br :xclose)
(with-output-to-string (str)
           (bptr-html:WITH-HTML-OUTPUT str
             (bptr-html:html
              (name :asd "asd" :dsd "asdds" :editable :amputable :pre-tag (pretag "pretag")
                    :post-tag "posttag" :pre-body (prebd.cls#id "Hi!") :post-body post-body
                    :body (tagname.cls#aid :asd "adsd"
                                           :body (:mpp :class "cls"
                                                      "eshse body"
                                                      (br)
                                                      (br :xclose nil)
                                                      (:every :asd dfd "asdasd" (:newline (:indent)) (tag.clcl "body"))
                                                      (:escape-every "<html>test</html>")(:newline)
                                                      (:escape (br))
                                                      ("tete" :body "ahaha")))))))
```

The output will be:

```html
<pretag>
  pretag
</pretag>

<name asd="asd" dsd="asdds" editable amputable>
  <prebd class="cls" id="id">
    Hi!
  </prebd>

  <tagname class="cls" id="aid" asd="adsd">
    <mpp class="cls">
      eshse body
      <br>
      </br>
      <br>
      </br>
      asddfdasdasd
      <tag class="clcl">
        body
      </tag>
      &lt;html&gt;test&lt;/html&gt;      
      &lt;br&gt;
      &lt;/br&gt;
      <tete>
        ahaha
      </tete>
    </mpp>
  </tagname>
  post-body
</name>
posttag
```

But note:

```
      &lt;html&gt;test&lt;/html&gt;      
 here comes 8 fantom spaces ;p --> ^^^^^^^
```

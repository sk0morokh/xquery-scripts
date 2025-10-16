xquery version "3.1";

declare namespace xsd = "http://www.w3.org/2001/XMLSchema";

declare %private function local:resolve-uri($ref as xs:string, $base as xs:string?) as xs:string {
  if (starts-with($ref, "file://")) then $ref
  else if ($base) then (
    let $base-dir := replace($base, "/[^/]*$", "/")
    return replace($base-dir || $ref, "\\", "/")
  ) else $ref
};

declare %private function local:get-prefix-for-namespace($schema as element()) as xs:string? {
  let $target-ns := $schema/@targetNamespace => string()
  return
    if ($target-ns) then (
      let $prefixes := in-scope-prefixes($schema)
      for $p in $prefixes
      where namespace-uri-for-prefix($p, $schema) = $target-ns
      return $p
    )[1]
    else ()
};

declare %private function local:get-namespace-uri($prefix as xs:string?, $context as element()) as xs:string? {
  if ($prefix = "") then $context/@targetNamespace => string()
  else if ($prefix) then namespace-uri-for-prefix($prefix, $context)
  else $context/@targetNamespace => string()
};

declare %private function local:normalize-qname($qname as xs:string, $context as element()) as xs:string? {
  if (contains($qname, ":")) then (
    let $parts := tokenize($qname, ":")
    let $prefix := $parts[1]
    let $local := $parts[2]
    let $uri := local:get-namespace-uri($prefix, $context)
    return if ($uri) then concat("{", $uri, "}", $local) else ()
  ) else (
    let $default-ns := local:get-namespace-uri("", $context)
    return if ($default-ns) then concat("{", $default-ns, "}", $qname) else $qname
  )
};

declare %private function local:extract-declared-types($schema as element()) as xs:string* {
  let $target-ns := $schema/@targetNamespace => string()
  let $simple-types := $schema/*[local-name() = 'simpleType' and @name] ! (
    if ($target-ns) then concat("{", $target-ns, "}", @name) else @name
  )
  let $complex-types := $schema/*[local-name() = 'complexType' and @name] ! (
    if ($target-ns) then concat("{", $target-ns, "}", @name) else @name
  )
  return ($simple-types, $complex-types)
};

declare %private function local:expand-type-references(
  $type-name as xs:string,
  $visited as map(*),
  $type-index as map(*)
) as xs:string* {
  if (map:contains($visited, $type-name)) then ()
  else (
    let $visited-new := map:put($visited, $type-name, true())
    let $def := map:get($type-index, $type-name)
    return
      if (empty($def)) then ()
      else (
        let $node := $def?node
        let $schema := $def?schema
        let $direct-references := (
          $node//@type,
          $node//@base,
          $node//self::*[local-name() = ('element', 'attribute')]/@type,
          $node//self::*[local-name() = ('restriction', 'extension')]/@base,
          $node//self::*[local-name() = 'list']/@itemType,
          $node//self::*[local-name() = 'union']/@memberTypes ! tokenize(., '\s+')
        ) ! string() ! local:normalize-qname(., $schema)

        let $nested-references := (
          for $ref in $direct-references
          return local:expand-type-references($ref, $visited-new, $type-index)
        )
        return ($direct-references, $nested-references)
      )
  )
};

declare %private function local:deep-extract-type-references($root as node(), $type-index as map(*)) as xs:string* {
  let $initial := map {
    "stack": $root,
    "result": ()
  }
  let $final := fold-left(
    1 to 10000,
    $initial,
    function($state, $_) {
      let $stack := $state?stack
      let $result := $state?result
      return
        if (empty($stack)) then $state
        else (
          let $current := $stack[1]
          let $rest-stack := subsequence($stack, 2)
          let $direct := (
            $current/@type,
            $current/@base,
            $current/self::*[local-name() = ('element', 'attribute')]/@type,
            $current/self::*[local-name() = ('restriction', 'extension')]/@base,
            $current/self::*[local-name() = 'list']/@itemType,
            $current/self::*[local-name() = 'union']/@memberTypes ! tokenize(., '\s+')
          ) ! string()

          let $children := $current/*
          let $new-stack := ($rest-stack, $children)
          let $context-element :=
            if ($current instance of element()) then $current
            else ($current/ancestor::*[1], $root/ancestor-or-self::*[1][. instance of element()])[1]

          let $normalized := for $ref in $direct return local:normalize-qname($ref, $context-element)
          let $expanded := (
            for $n in $normalized
            return local:expand-type-references($n, map {}, $type-index)
          )
          let $new-result := ($result, $normalized, $expanded)
          return map {
            "stack": $new-stack,
            "result": $new-result
          }
        )
    }
  )
  return distinct-values($final?result[. instance of xs:string and not(starts-with(., "{http://www.w3.org/2001/XMLSchema}"))])
};


let $main-files := (
  "file:///C:/УКАЗАТЬ_ПУТЬ_ДО_СХЕМ_ВЛОЖЕННЫХ",

)

let $initial-state := map {
  "cache": map {},
  "queue": $main-files
}

let $final-state :=
  fold-left(
    1 to 1000,
    $initial-state,
    function($state, $_) {
      let $cache := $state?cache
      let $queue := $state?queue
      return
        if (empty($queue)) then $state
        else (
          let $current-uri := $queue[1]
          let $rest-queue := subsequence($queue, 2)
          return
            if (map:contains($cache, $current-uri)) then map { "cache": $cache, "queue": $rest-queue }
            else (
              let $doc := doc($current-uri)
              let $schema := $doc/*[1]
              let $new-cache := map:put($cache, $current-uri, $schema)
              let $includes := $schema/*[local-name() = "include"]/@schemaLocation ! string()
              let $resolved-includes := for $inc in $includes return local:resolve-uri($inc, $current-uri)
              let $new-queue := (
                for $item in ($rest-queue, $resolved-includes)
                where not(map:contains($new-cache, $item))
                return $item
              )
              return map { "cache": $new-cache, "queue": $new-queue }
            )
        )
    }
  )

let $all-schemas-cache := $final-state?cache
let $all-schemas := map:keys($all-schemas-cache) ! map:get($all-schemas-cache, .)
let $ndc-schema-uri := "file:///C:/ПУТЬ_ОСНОВНОЙ_СХЕМЫ"
let $ndc-schema := map:get($all-schemas-cache, $ndc-schema-uri)
let $other-schemas := $all-schemas[not(. is $ndc-schema)]

let $type-index := map:merge(
  for $schema in $all-schemas
  for $type-def in $schema/*[local-name() = ('simpleType', 'complexType') and @name]
  let $ns := $schema/@targetNamespace => string()
  let $full-name := if ($ns) then concat("{", $ns, "}", $type-def/@name) else $type-def/@name
  return map:entry($full-name, map { "node": $type-def, "schema": $schema })
)

let $ndc-declared-types := local:extract-declared-types($ndc-schema)

let $other-type-references := distinct-values(
  for $s in $other-schemas
  return local:deep-extract-type-references($s, $type-index)
)

let $unused-types := $ndc-declared-types[not(. = $other-type-references)]

return
  <result>
    <summary>Declared in NDC_BRMVP.xsd but NOT USED elsewhere: {count($unused-types)}</summary>
    <unusedTypes>
    {
      for $t in $unused-types
      return <type>{replace($t, '^\{[^}]*\}', '')}</type>
    }
    </unusedTypes>
  </result>
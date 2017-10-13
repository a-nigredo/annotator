Application annotates top level entity(trait, class, object) if it finds logger call insdie.

#### Configuration
**-p** - path to scala files or single file<br />
**-f** - logger field name. default: "logger"<br />
**-a** - annotation<br />

#### Placeholder
**_NL_** - new line

#### Example

```
package com.example

class Foo extends LazyLogging {
    def bar = logger.info("log me")
}

package com.example.annotation

class BarAnnotation extends StaticAnnotation
```

After run:
```
java -jar annotator -p="com.example.Foo" -f="logger" -a="com.example.annotation.BarAnnotation_NL_"
```
will be:
```
package com.example

import com.example.annotation.BarAnnotation

@BarAnnotation
class Foo extends LazyLogging {
    def bar = logger.info("log me")
}

```
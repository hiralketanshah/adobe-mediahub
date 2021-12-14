***********************
FoundationUI Shell Sort
***********************

.. granite:servercomponent:: /libs/granite/ui/components/shell/collectionpage/sort

   The component to provide sorting in Shell CollectionPage. For smooth functioning, the collectionpage views datasource should be configured to provide sorted data based on different options.

   It has the following content structure:

   .. gnd:gnd::

      [granite:ShellSort] > granite:commonAttrs

      /**
       * To displayed sort options in lexicographically order
       */
      - ordered (Boolean) = false

      /**
       * To override/remove the default sortBy options provide by foundationUI. This will not remove "none" option
       */
      - override (boolean) = false

      /**
       * path to the additional sort options. The provided path resource must have a `items` child containing all options.
       */
      - configPath (String)

   Each options provided in configPath following content structure:

   .. gnd:gnd::

      [granite:ShellSortBy]

      /**
       * The item display text .
       */
      - text (String) mandatory i18n

      /**
       * The item value.
       * To be compliant with table sorting. This should be same as table layout sortable column name.
       */
      - value (String) mandatory

   Example [Sort Component] ::
      + collectionpage
        - sling:resourceType = "libs/granite/ui/components/shell/collectionpage"
        + actions
          + secondary
            + sort
              - ordered = true
              - sling:resourceType = "granite/ui/components/shell/collectionpage/sort"
              - configPath = "${Additional SortBy Options Path}"


   Example [configPath] ::
        + items
          + item 1
            - text = "Item 1"
            - value = "item1"
          + item 2
            - text = "Item 2"
            - value = "item2"


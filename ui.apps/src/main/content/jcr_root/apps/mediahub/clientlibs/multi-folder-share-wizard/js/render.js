(function ($, $document) {
    $document.on("foundation-contentloaded", init);

    function init() {
      if($("[name='./jcr:content/metadata/bnpp-title-en']") && $("[name='./jcr:content/metadata/bnpp-title-en']")[0]) {
		    $("[name='./jcr:content/metadata/bnpp-title-en']")[0].disabled = true;
		    $("[name='./jcr:content/metadata/bnpp-title-en']")[0].placeholder = Granite.I18n.get("Non Editable Title");
      }

      if($("[name='./jcr:content/metadata/bnpp-title-local']") && $("[name='./jcr:content/metadata/bnpp-title-local']")[0]) {
        $("[name='./jcr:content/metadata/bnpp-title-local']")[0].disabled = true;
        $("[name='./jcr:content/metadata/bnpp-title-local']")[0].placeholder = Granite.I18n.get("Non Editable Title");
      }
    }

}(jQuery, jQuery(document)));
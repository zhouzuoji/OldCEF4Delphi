object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Initializing browser. Please wait...'
  ClientHeight = 540
  ClientWidth = 801
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonPnl: TPanel
    Left = 0
    Top = 0
    Width = 801
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    Caption = 'ButtonPnl'
    Enabled = False
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    ShowCaption = False
    TabOrder = 0
    object NavButtonPnl: TPanel
      Left = 5
      Top = 5
      Width = 183
      Height = 25
      Align = alLeft
      BevelOuter = bvNone
      ShowCaption = False
      TabOrder = 1
      object BackBtn: TButton
        Left = 63
        Top = 0
        Width = 25
        Height = 25
        Caption = '3'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = BackBtnClick
      end
      object ForwardBtn: TButton
        Left = 93
        Top = 0
        Width = 25
        Height = 25
        Caption = '4'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = ForwardBtnClick
      end
      object ReloadBtn: TButton
        Left = 123
        Top = 0
        Width = 25
        Height = 25
        Caption = 'q'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        OnClick = ReloadBtnClick
      end
      object StopBtn: TButton
        Left = 153
        Top = 0
        Width = 25
        Height = 25
        Caption = '='
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        OnClick = StopBtnClick
      end
      object AddTabBtn: TButton
        Left = 1
        Top = 0
        Width = 25
        Height = 25
        Caption = '+'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = AddTabBtnClick
      end
      object RemoveTabBtn: TButton
        Left = 32
        Top = 0
        Width = 25
        Height = 25
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = RemoveTabBtnClick
      end
    end
    object ConfigPnl: TPanel
      Left = 764
      Top = 5
      Width = 32
      Height = 25
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object GoBtn: TButton
        Left = 6
        Top = 0
        Width = 25
        Height = 25
        Caption = #9658
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = GoBtnClick
      end
    end
    object URLEditPnl: TPanel
      Left = 188
      Top = 5
      Width = 576
      Height = 25
      Align = alClient
      BevelOuter = bvNone
      Padding.Top = 2
      ShowCaption = False
      TabOrder = 0
      object URLCbx: TComboBox
        Left = 0
        Top = 2
        Width = 576
        Height = 21
        Align = alClient
        TabOrder = 0
        Text = 'https://www.baidu.com'
        Items.Strings = (
          'https://www.baidu.com'
          
            'https://www.whatismybrowser.com/detect/what-http-headers-is-my-b' +
            'rowser-sending'
          'http://www.adobe.com/software/flash/about/'
          'http://isflashinstalled.com/'
          'chrome://version/'
          'http://html5test.com/'
          'http://webglsamples.org/'
          'https://html5demos.com/drag/')
      end
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 35
    Width = 801
    Height = 364
    Align = alClient
    TabOrder = 1
    OnChange = PageControl1Change
  end
  object pnl1: TPanel
    Left = 0
    Top = 399
    Width = 801
    Height = 141
    Align = alBottom
    Caption = 'pnl1'
    TabOrder = 2
    object btn1: TSpeedButton
      Left = 712
      Top = 1
      Width = 88
      Height = 139
      Align = alRight
      OnClick = btn1Click
      ExplicitLeft = 808
      ExplicitHeight = 50
    end
    object mmoScript: TMemo
      Left = 1
      Top = 1
      Width = 711
      Height = 139
      Align = alClient
      Lines.Strings = (
        'window.doOnChange=function(element) {'
        '  var ev = document.createEvent("HTMLEvents");'
        '  ev.initEvent("change", false, true);'
        '  element.dispatchEvent(ev);'
        '}'
        ''
        'window.buy=function(msg, cnt) {'
        '  var inputs = document.getElementsByTagName("input");'
        '  var msgFilled = !msg;'
        '  var cntFilled = !cnt;'
        '  if (!msgFilled || !cntFilled) {'
        '    for (var i in inputs) {'
        '      var input = inputs[i];'
        
          '      if (!msgFilled && input.placeholder.indexOf("'#22635#20889#20869#23481#24050#21644#21334#23478#21327#21830#30830#35748'"' +
          ') >= 0) {'
        '      '#9'alert("'#25214#21040#30041#35328#26694'");'
        '        input.value = msg;'
        '        msgFilled=true;'
        '      }'
        
          '      else if (!cntFilled && input.type == "number" && input.cla' +
          'ssName == "amount") {'
        '      '#9'alert("'#25214#21040#36141#20080#25968#37327#26694'");'
        '        input.value = cnt;'
        '        cntFilled=true;'
        '      }'
        '      if (msgFilled && cntFilled) break;'
        '    }'
        ''
        '    if (!msgFilled || !cntFilled)'
        '      return false;'
        '  }'
        ''
        
          '  var divs = document.getElementsByClassName("mui-flex align-cen' +
          'ter");'
        '  for (var i = divs.length - 1; i >= 0; --i) {'
        '    div = divs[i];'
        '    var spans = div.getElementsByTagName("span");'
        '    if (spans.length == 0)'
        '      continue;'
        '    for (var j = 0; j < spans.length; ++j) {'
        '      var span = spans[j];'
        '      if (span.title == "'#25552#20132#35746#21333'") {'
        '      '#9'alert("'#25214#21040#25552#20132#35746#21333#25353#38062'");'
        '        span.click();'
        '        return true;'
        '      }'
        '    }'
        '  }'
        ''
        '  return false;'
        '}'
        ''
        'window.pay2=function(password) {'
        '  if (!password)'
        '    return false;'
        '  var pwds = document.getElementsByName("pwd_unencrypt");'
        '  if (pwds.length != 1)'
        '    return false;'
        '  pwds[0].value = password;'
        '  var btns = document.getElementsByTagName("button");'
        '  for (var i in btns) {'
        '    var btn = btns[i];'
        '    if (btn.innerText == "'#30830#23450'") {'
        '      alert("'#25214#21040#30830#23450#20184#27454#25353#38062'");'
        '      btn.click();'
        '      return true;'
        '    }'
        '  }'
        '  return false;'
        '}'
        ''
        'window.pay1=function() {'
        '  var btns = document.getElementsByTagName("button");'
        '  for (var i in btns) {'
        '    var btn = btns[i];'
        '    if (btn.innerText == "'#30830#35748#20184#27454'") {'
        '      alert("'#25214#21040#30830#35748#20184#27454#25353#38062'");'
        '      btn.click();'
        '      return true;'
        '    }'
        '  }'
        '  return false;'
        '}'
        ''
        'window.checkPage= function(msg, cnt, password) {'
        
          '  // https://h5.m.taobao.com/cart/order.html?itemId=575070622805' +
          '&item_num_id=575070622805&_input_charset=utf-8&buyNow=true&v=0&q' +
          'uantity=1&skuId='
        
          '  // https://maliprod.alipay.com/w/trade_pay.do?tcode=eyJiaXpPcm' +
          'RlcklkcyI6IjE5NjcxNzM0MTg3MTg1Njc2NyIsImJ1eWVySWQiOiIyNDIzODU2Nz' +
          'Y3IiwidHlwZSI6IjMifQ==&alipay_trade_no=2018080721001001020562831' +
          '768&s_id=36eea86006a10fb634d6c206b746cc4f&return_url=https%3A%2F' +
          '%2Fmarket.m.taobao.com%2Fapps%2Fmarket%2Ftrade%2Findex.html%3Fwh' +
          '_weex%3Dtrue%26wx_navbar_transparent%3Dtrue%26orderIds%3D1967173' +
          '41871856767%26degrade%3D0%26act%3Dfalse%26spm%3Da220l.10694284.0' +
          '.i3&pay_order_id=196717341871856767'
        
          '  // https://maliprod.alipay.com/w/trade_pay.do?tcode=eyJiaXpPcm' +
          'RlcklkcyI6IjE5NjcxNzM0MTg3MTg1Njc2NyIsImJ1eWVySWQiOiIyNDIzODU2Nz' +
          'Y3IiwidHlwZSI6IjMifQ==&alipay_trade_no=2018080721001001020562831' +
          '768&s_id=36eea86006a10fb634d6c206b746cc4f&return_url=https%3A%2F' +
          '%2Fmarket.m.taobao.com%2Fapps%2Fmarket%2Ftrade%2Findex.html%3Fwh' +
          '_weex%3Dtrue%26wx_navbar_transparent%3Dtrue%26orderIds%3D1967173' +
          '41871856767%26degrade%3D0%26act%3Dfalse%26spm%3Da220l.10694284.0' +
          '.i3&pay_order_id=196717341871856767'
        
          '  // https://mclient.alipay.com/h5/cashierPay.htm?awid=RZ25MOX9c' +
          'lRSTl9M26KqSaSdUT0DfGmobileclientgwRZ25'
        '  var pathname=document.location.pathname;'
        '  if (pathname == "/h5/cashierPay.htm") {'
        '    return true;'
        '  }'
        '  if (pathname == "/cart/order.html") {'
        '    return buy(msg, cnt);'
        '  }'
        '  if (pathname == "/w/trade_pay.do") {'
        '    return pay2(password);'
        '  }'
        '  return false;'
        '}')
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
end

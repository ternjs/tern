/*global define tern*/
(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    return mod(require("../lib/tern"), require("./node_resolve"));
  if (typeof define == "function" && define.amd) // AMD
    return define(["../lib/tern", "./node_resolve"], mod);
  mod(tern, tern);
})(function(tern) {
  "use strict"

  tern.registerPlugin("wxapp", function(server) {
    server.loadPlugin("node_resolve")
    server.on("postReset", function() {
      var mods = server.mod.modules, locals = server.cx.definitions.node
      for (var name in locals) if (/^[a-z_]*$/.test(name))
        mods.knownModules[name] = locals[name]
    })
    server.addDefs(defs)
  })

  var defs = {
    "!name": "wxapp",
    "!define": {
    },
    wx: {
      request: {
        "!type": "fn(url: string, data: object|string)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/network-request.html",
        "!doc": "发起https请求。一个微信小程序，同时只能有5个网络请求连接。"
      },
      uploadFile: {
        "!type": "fn(url: string, filePath: string, name: string)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/network-file.html#wxuploadfileobject",
        "!doc": "将本地资源上传到开发者服务器"
      },
      downloadFile: {
        "!type": "fn(url: string, type: string)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/network-file.html#wxdownloadfileobject",
        "!doc": "下载文件资源到本地。"
      },
      connectSocket: {
        "!type": "fn(url: string, data: object)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/network-socket.html",
        "!doc": "下载文件资源到本地。"
      },
      onSocketOpen: {
        "!type": "fn(callback: fn(res: object))",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/network-socket.html#wxconnectsocketobject",
        "!doc": "监听WebSocket连接打开事件。"
      },
      onSocketError: {
        "!type": "fn(callback: fn(res: object))",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/network-socket.html?#wxonsocketerrorcallback",
        "!doc": "监听WebSocket错误。"
      },
      sendSocketMessage: {
        "!type": "fn(data: object)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/network-socket.html#wxsendsocketmessageobject",
        "!doc": "通过 WebSocket 连接发送数据，需要先 wx.connectSocket，并在 wx.onSocketOpen 回调之后才能发送。"
      },
      onSocketMessage: {
        "!type": "fn(callback: fn(res: object))",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/network-socket.html#wxsendsocketmessageobject",
        "!doc": "监听WebSocket接受到服务器的消息事件。"
      },
      closeSocket: {
        "!type": "fn()",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/network-socket.html#wxclosesocket",
        "!doc": "关闭WebSocket连接"
      },
      onSocketClose: {
        "!type": "fn(callback: fn())",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/network-socket.html?#wxonsocketclosecallback",
        "!doc": "监听WebSocket关闭。"
      },
      chooseImage: {
        "!type": "fn(success: fn())",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-picture.html#wxchooseimageobject",
        "!doc": "从本地相册选择图片或使用相机拍照。"
      },
      previewImage: {
        "!type": "fn(urls: string|array, current: string)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-picture.html?#wxpreviewimageobject",
        "!doc": "从本地相册选择图片或使用相机拍照。"
      },
      startRecord: {
        "!type": "fn(sucess: fn(res: object))",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-record.html?#wxstartrecordobject",
        "!doc": "开始录音。当主动调用wx.stopRecord，或者录音超过1分钟时自动结束录音，返回录音文件的临时文件路径"
      },
      stopRecord: {
        "!type": "fn()",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-record.html?#wxstoprecord",
        "!doc": "主动调用停止录音。"
      },
      playVoice: {
        "!type": "fn(filePath: string)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-voice.html#wxplayvoiceobject",
        "!doc": "开始播放语音，同时只允许一个语音文件正在播放，如果前一个语音文件还没播放完，将中断前一个语音播放。"
      },
      pauseVoice: {
        "!type": "fn()",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-voice.html#wxpausevoice",
        "!doc": "暂停正在播放的语音。再次调用wx.playVoice播放同一个文件时，会从暂停处开始播放。"
      },
      stopVoice: {
        "!type": "fn()",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-voice.html#wxstopvoice",
        "!doc": "结束播放语音。"
      },
      getBackgroundAudioPlayerState: {
        "!type": "fn(success: fn(res: object))",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-background-audio.html?#wxgetbackgroundaudioplayerstateobject",
        "!doc": "获取音乐播放状态。"
      },
      playBackgroundAudio: {
        "!type": "fn(dataUrl: string, title: string, coverImgUrl: string)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-background-audio.html#wxplaybackgroundaudioobject",
        "!doc": "播放音乐，同时只能有一首音乐正在播放。"
      },
      pauseBackgroundAudio: {
        "!type": "fn()",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-background-audio.html?#wxpausebackgroundaudio",
        "!doc": "暂停播放音乐。"
      },
      seekBackgroundAudio: {
        "!type": "fn(position: number)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-background-audio.html#wxseekbackgroundaudioobject",
        "!doc": "控制音乐播放进度。"
      },
      stopBackgroundAudio: {
        "!type": "fn()",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-background-audio.html?#wxstopbackgroundaudio",
        "!doc": "停止播放音乐。"
      },
      onBackgroundAudioPlay: {
        "!type": "fn(callback: fn())",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-background-audio.html#wxonbackgroundaudioplaycallback",
        "!doc": "监听音乐播放。"
      },
      onBackgroundAudioPause: {
        "!type": "fn(callback: fn())",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-background-audio.html#wxonbackgroundaudiopausecallback",
        "!doc": "监听音乐暂停。"
      },
      onBackgroundAudioStop: {
        "!type": "fn(callback: fn())",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-background-audio.html#wxonbackgroundaudiostopcallback",
        "!doc": "监听音乐停止。"
      },
      saveFile: {
        "!type": "fn(tempFilePath: string)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/file.html",
        "!doc": "保存文件到本地。"
      },
      chooseVideo: {
        "!type": "fn(success: fn(res: object))",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/media-video.html",
        "!doc": "拍摄视频或从手机相册中选视频，返回视频的临时文件路径。"
      },
      setStorage: {
        "!type": "fn(key: string, data: string|object, success: fn())",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/data.html?#wxsetstorageobject",
        "!doc": "将数据存储在本地缓存中指定的 key 中，会覆盖掉原来该 key 对应的内容，这是一个异步接口。"
      },
      setStorageSync: {
        "!type": "fn(key: string, data: string|object)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/data.html?#wxsetstoragesynckeyvalue",
        "!doc": "将 data 存储在本地缓存中指定的 key 中，会覆盖掉原来该 key 对应的内容，这是一个同步接口。"
      },
      getStorage: {
        "!type": "fn(key: string, success: fn())",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/data.html#wxgetstorageobject",
        "!doc": "从本地缓存中异步获取指定 key 对应的内容。"
      },
      getStorageSync: {
        "!type": "fn(key: string)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/data.html#wxgetstoragesynckey",
        "!doc": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/data.html#wxgetstoragesynckey"
      },
      clearStorage: {
        "!type": "fn()",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/data.html#wxclearstorage",
        "!doc": "清理本地数据缓存。"
      },
      clearStorageSync: {
        "!type": "fn()",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/data.html#wxclearstoragesync",
        "!doc": "同步清理本地数据缓存"
      },
      getLocation: {
        "!type": "fn(type: string, success: fn())",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/location.html",
        "!doc": "获取当前的地理位置、速度。"
      },
      openLocation: {
        "!type": "fn(latitude: float, longitude: float)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/location.html#wxopenlocationobject",
        "!doc": "使用微信内置地图查看位置"
      },
      getNetworkType: {
        "!type": "fn(success: fn())",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/device.html?#wxgetnetworktypeobject",
        "!doc": "获取网络类型。"
      },
      getSystemInfo: {
        "!type": "fn(success: fn())",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/device.html#wxgetsysteminfoobject",
        "!doc": "获取系统信息。"
      },
      onAccelerometerChange: {
        "!type": "fn(callback: fn())",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/device.html#wxonaccelerometerchangecallback",
        "!doc": "监听重力感应数据，频率：5次/秒"
      },
      onCompassChange: {
        "!type": "fn(callback: fn())",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/device.html#wxoncompasschangecallback",
        "!doc": "监听罗盘数据，频率：5次/秒"
      },
      setNavigationBarTitle: {
        "!type": "fn(title: string)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/ui.html",
        "!doc": "动态设置当前页面的标题。"
      },
      showNavigationBarLoading: {
        "!type": "fn()",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/ui.html",
        "!doc": "在当前页面显示导航条加载动画。"
      },
      hideNavigationBarLoading: {
        "!type": "fn()",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/ui.html",
        "!doc": "隐藏导航条加载动画。"
      },
      navigateTo: {
        "!type": "fn(url: string)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/ui-navigate.html",
        "!doc": "保留当前页面，跳转到应用内的某个页面。"
      },
      redirectTo: {
        "!type": "fn(url: string)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/ui-navigate.html#wxredirecttoobject",
        "!doc": "关闭当前页面，跳转到应用内的某个页面。"
      },
      navigateBack: {
        "!type": "fn()",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/ui-navigate.html#wxnavigateback",
        "!doc": "关闭当前页面，回退前一页面。"
      },
      createAnimation: {
        "!type": "fn()",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/api-animation.html",
        "!doc": "创建一个动画实例animation。调用实例的方法来描述动画。最后通过动画实例的export方法导出动画数据传递给组件的animation属性。"
      },
      createContext: {
        "!type": "fn()",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/api-canvas.html",
        "!doc": "创建并返回绘图上下文context对象。"
      },
      drawCanvas: {
        "!type": "fn(canvasId: string, actions: array)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/api-canvas.html#wxdrawcanvasobject",
        "!doc": "创建绘图"
      },
      hideKeyboard: {
        "!type": "fn()",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/ui-other.html",
        "!doc": "收起键盘"
      },
      login: {
        "!type": "fn(success: fn())",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/api-login.html",
        "!doc": "调用接口获取登录凭证（code）进而换取用户登录态信息。"
      },
      getUserInfo: {
        "!type": "fn(success: fn())",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/open.html#wxgetuserinfoobject",
        "!doc": "获取用户信息，需要先调用 wx.login 接口"
      },
      requestPayment: {
        "!type": "fn(timeStamp: number, nonceStr: string, package: string, signType: string, paySign: string)",
        "!url": "https://mp.weixin.qq.com/debug/wxadoc/dev/api/api-pay.html#wxrequestpaymentobject",
        "!doc": "发起微信支付。"
      }
    }
  };
});

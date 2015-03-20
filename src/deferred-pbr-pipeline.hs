yDeferredLighting
  :: (HasScene a DeferredEntity DeferredEnvironment, HasHDRCamera a, HasDeferredSettings a, DeferredMonad m env)
  => YageResource (RenderSystem m a (Texture2D PixelRGB8))
yDeferredLighting = do
  throwWithStack $ glEnable GL_FRAMEBUFFER_SRGB
  throwWithStack $ buildNamedStrings embeddedShaders ((++) "/res/glsl/")
  drawGBuffer     <- gPass
  skyPass         <- drawSky
  defaultRadiance <- textureRes (pure (defaultMaterialSRGB^.materialTexture) :: Cubemap (Image PixelRGB8))
  voxelize        <- voxelizePass 256 256 256
  visVoxel        <- visualizeVoxelPass
  drawLights      <- lightPass
  postAmbient     <- postAmbientPass
  renderBloom     <- addBloom
  tonemapPass     <- toneMapper
  debugOverlay    <- toneMapper

  return $ proc input -> do
    mainViewport  <- sysEnv viewport    -< ()

    -- render surface attributes for lighting out
    gbufferTarget <- autoResized mkGbufferTarget           -< mainViewport^.rectangle
    gBuffer       <- processPassWithGlobalEnv drawGBuffer  -< ( gbufferTarget
                                                              , input^.scene
                                                              , input^.hdrCamera.camera )
    -- voxelize for ambient occlusion
    mVoxelOcclusion <- if input^.deferredSettings.activeVoxelAmbientOcclusion
      then fmap Just voxelize -< input
      else pure Nothing -< ()
    voxelSceneTarget <- autoResized mkVisVoxelTarget -< mainViewport^.rectangle

    -- lighting
    lBufferTarget <- autoResized mkLightBuffer -< mainViewport^.rectangle
    _lBuffer   <- processPassWithGlobalEnv drawLights  -< ( lBufferTarget
                                                          , input^.scene.environment.lights
                                                          , input^.hdrCamera.camera
                                                          , gBuffer )

    -- ambient
    let radiance = maybe defaultRadiance (view $ materials.radianceMap.materialTexture) (input^.scene.environment.sky)
    post      <- processPassWithGlobalEnv postAmbient -< ( lBufferTarget
                                                         , radiance
                                                         , mVoxelOcclusion
                                                         , input^.hdrCamera.camera
                                                         , gBuffer )

    -- sky pass
    skyTarget <- onChange  -< (post, gBuffer^.depthChannel)
    sceneTex <- if isJust $ input^.scene.environment.sky
      then skyPass -< ( fromJust $ input^.scene.environment.sky
                      , input^.hdrCamera.camera
                      , skyTarget
                      )
      else returnA -< post

    -- bloom pass
    bloomed   <- renderBloom -< (input^.hdrCamera.bloomSettings, sceneTex)

    -- tone map from hdr (floating) to discrete Word8
    finalScene <- tonemapPass -< (input^.hdrCamera.hdrSensor, sceneTex, Just bloomed)
    if input^.deferredSettings.showDebugOverlay && isJust mVoxelOcclusion
      then do
        visVoxTex <- processPassWithGlobalEnv visVoxel -< ( voxelSceneTarget
                                                          , fromJust mVoxelOcclusion
                                                          , input^.hdrCamera.camera
                                                          , [VisualizeSceneVoxel,VisualizePageMask] )
        debugOverlay -< (input^.hdrCamera.hdrSensor, finalScene, Just visVoxTex)
      else returnA -< finalScene

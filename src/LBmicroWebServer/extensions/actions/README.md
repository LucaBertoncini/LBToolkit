# LBWebActions (optional extension)

`LBWebActions` adds reusable endpoint/action dispatch to `LBmicroWebServer`
without changing the core router.

The host application supplies action definitions with:

- a strict kebab-case `Code`;
- an opaque `AuthorizationToken`;
- optional opaque key/value `Metadata`.

The extension does **not** read `Routes.xml` and does not interpret capability,
role, group, `Implies`, assignability or AI concepts.

## Units

- `uLBActionTypes`: definitions, opaque metadata, authorization callback and
  aggregated startup errors;
- `uLBActionNames`: kebab-case validation and `DoXxx` / `DescribeXxx` names;
- `uLBActionDispatcher`: JSON `action` dispatch, 400/403 responses, duplicate
  checks and `TStandardWorker` invocation;
- `uLBActionBinder`: RTTI binding through published `DoXxx` methods;
- `uLBActionGuard`: deny-by-default authorization and implicit-action guard.

## Minimal use

```pascal
Errors := TLBActionConfigurationErrors.Create;
Dispatcher := TLBActionDispatcher.Create('orders', 'Requests',
  @Authorizer.Authorize, Errors);
Dispatcher.AddDefinition('list', 'orders.read');
Dispatcher.AddDefinition('set-result', 'orders.write');
TLBActionBinder.BindObject(Dispatcher, WorkerObject);
Dispatcher.ValidateBindings;
Errors.RaiseIfAny('Invalid action configuration');
```

The bound object publishes `DoList` and `DoSetResult`, both with the framework's
`TStandardWorker` signature. For body-less endpoint families, create one
`TLBActionDefinition` and protect the application adapter with
`TLBActionGuard`.

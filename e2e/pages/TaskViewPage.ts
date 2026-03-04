import { Page, Locator, expect } from '@playwright/test';

export class TaskViewPage {
    readonly page: Page;
    readonly doneButton: Locator;

    constructor(page: Page) {
        this.page = page;
        this.doneButton = page.locator('button.done');
    }

    async verifyTaskContent(expectedText: string) {
        await expect(this.page.locator('body')).toContainText(expectedText);
        await expect(this.doneButton).toBeVisible();
    }
}
